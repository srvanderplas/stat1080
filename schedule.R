library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)

# Create a calendar for your syllabus
# Source: http://svmiller.com/blog/2020/08/a-ggplot-calendar-for-your-semester/

date_seq <- function(tbl) {
  stopifnot("start" %in% names(tbl))
  stopifnot("end" %in% names(tbl))

  if (is.na(tbl$end)) {
    ymd(tbl$start)
  } else {
    seq(ymd(tbl$start), ymd(tbl$end), by = 1)
  }
}


library(yaml)
semester_info <- read_yaml("schedule.yaml")
due_dates <- semester_info$`important-course-dates` |>
  purrr::map_dfr(tibble::as_tibble) |>
  dplyr::mutate(date = ymd(date), category = "due")

schedule <- semester_info$schedule |>
  purrr::map_dfr(tibble::as_tibble)

dates <- purrr::map_dfr(semester_info$semester, as_tibble) |>
  pivot_longer(everything(), names_to = "type", values_to = "date") |>
  filter(!is.na(date)) |>
  mutate(date = ymd(date))


# Weekday(s) of class
class_wdays <- semester_info$`class-days`

# What are the full dates of the semester? Monday of week 1 - end of finals
semester_dates <- dates |>
  filter(type %in% c("start", "end")) |>
  pivot_wider(names_from = "type", values_from = "date") |>
  date_seq()

# Dates that are school holidays/no class by university fiat
not_here_dates <- dates |>
  filter(type == "holiday") |>
  pluck("date")

exam_week <- dates |>
  filter(type == "exams") |>
  pluck("date")

# Custom function for treating the first day of the month as the first week
# of the month up until the first Sunday (unless Sunday was the start of the month)
wom <- function(date) {
  first <- wday(as.Date(paste(year(date), month(date), 1, sep = "-")))
  return((mday(date) + (first - 2)) %/% 7 + 1)
}

hwk_dates <- filter(due_dates, str_detect(id, "hwk"))
proj_dates <- filter(due_dates, str_detect(id, "project"))

# Create a data frame of dates, assign to Cal
Cal <- tibble(date = seq(floor_date(min(semester_dates), "month"), ceiling_date(max(semester_dates), "month") - days(1), by = 1)) %>%
  mutate(
    mon = lubridate::month(date, label = T, abbr = F), # get month label
    wkdy = weekdays(date, abbreviate = T), # get weekday label
    wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), # make sure Sunday comes first
    semester = date %in% semester_dates, # is date part of the semester?
    due = date %in% hwk_dates$date, # is it a due date?
    project = date %in% proj_dates$date,
    not_here = date %in% not_here_dates, # is it a day off?
    exam_wk = date %in% exam_week,
    day = lubridate::mday(date), # get day of month to add later as a label
    # Below: our custom wom() function
    week = wom(date),
    year_wk = epiweek(date)
  )

# Create a category variable, for filling in squares colorwise

Cal <- Cal %>%
  mutate(category = case_when(
    due ~ "Hwk Due Date",
    project ~ "Project Due Date",
    not_here ~ "Holiday",
    exam_wk ~ "Finals",
    semester & wkdy %in% class_wdays ~ "Class",
    semester ~ "Semester",
    TRUE ~ "NA"
  )) |>
  left_join(due_dates, by = c("date", "category")) |>
  mutate(sem_week = year_wk - min(year_wk[semester])+1) |>
  mutate(sem_week = if_else(sem_week<1, NA, sem_week)) |>
  mutate(sem_week = if_else(sem_week>16, NA, sem_week)) 

trimmable_weeks <- tibble(mon = c("August", "December", "January", "May"), week = c(1, 6, 1, 6))

trim_weeks <- Cal |>
  select(mon, week, year_wk) |>
  group_by(mon, week) |>
  mutate(days = n()) |>
  group_by(mon) |>
  mutate(m_max = max(week)) |>
  ungroup() |>
  mutate(m_mean = mean(m_max)) |>
  filter(m_max > m_mean) |> # Don't filter out the first/last week if all months have same weeks
  filter(m_max == max(m_max), days <=2) |>
  inner_join(trimmable_weeks)

if (nrow(trim_weeks)<=2) {
  Cal <- anti_join(Cal, trim_weeks)
}

class_cal <- ggplot(Cal, aes(wkdy, week)) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    # legend.position = "inside",
    # legend.position.inside = c(1, 0),
    # legend.justification = c(1, 0),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
  ) +
  # geom_tile and facet_wrap will do all the heavy lifting
  geom_tile(alpha = 0.8, aes(fill = category), color = "black", linewidth = .45) +
  facet_wrap(~mon, scales = "free", ncol = 5) +
  # fill in tiles to make it look more "calendary" (sic)
  geom_text(aes(label = day, color = semester & (!not_here))) +
  # put your y-axis down, flip it, and reverse it
  scale_y_reverse(breaks = NULL) +
  # manually fill scale colors to something you like...
  scale_color_manual(values = c("FALSE" = "grey80", "TRUE" = "black"), guide = "none") +
  scale_fill_manual(
    values = c(
      "Class" = "#9febfe",
      "Hwk Due Date" = "#f6bd17",
      "Project Due Date" = "#90b6a8",
      "Semester" = "white",
      "Finals" = "#CCCCCC",
      "Holiday" = "#0F2439",
      "NA" = "white" # I like these whited out...
    ),
    # ... but also suppress a label for a non-class semester day
    breaks = c("Holiday", "Hwk Due Date", "Project Due Date", "Class", "Finals")
  )
# class_cal

topics <- schedule |>
  mutate(sem_week = 1:n()) |>
  select(sem_week, name)

cal_dates <- Cal |>
  filter(!is.na(sem_week)) |>
  filter(as.numeric(wkdy) %in% 2:6) |>
  group_by(sem_week) |>
  summarize(date = min(date))


duedates <- due_dates |>
  left_join(select(Cal, date, sem_week), by = "date") |>
  mutate(important = name) |>
  select(sem_week, date, important)

scheduletbl <- topics |>
  left_join(cal_dates) |>
  rename(start_date = date) |>
  full_join(duedates) |>
  arrange(sem_week) |>
  group_by(sem_week) |>
  fill("name") |>
  nest(duedates = -c(sem_week, name, start_date)) |>
  mutate(duedates = map(duedates, function(df) {
    df |>
      group_by(important) |>
      mutate(
        datestr = paste(format.Date(date, "%m-%d"), collapse = ", "),
        text = if_else(is.na(important),
          NA_character_,
          sprintf("%s (%s)", important, datestr)
        )
      ) |>
      select(-date, -datestr) |>
      ungroup() |>
      unique()
  })) |>
  unnest(c(duedates)) |>
  mutate("Week Start" = format.Date(start_date, "%m-%d")) |>
  select(-start_date, -important) |>
  rename("Week" = sem_week, "Topic" = name, "Important Dates" = text) |>
  mutate(weeksort = Week) |>
  mutate(weeksort = if_else(Topic == "Spring Break", 9.5, weeksort)) |>
  # mutate(
  #   Week = if_else(weeksort > 9.5, Week - 1, Week),
  #   weeksort = if_else(weeksort > 9.5, weeksort - 1, weeksort)
  # ) |>
  arrange(weeksort) |>
  mutate(Week = if_else(Topic == "Spring Break", NA, Week))

# schedule
