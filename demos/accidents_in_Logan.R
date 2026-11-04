library(jsonlite)
library(ggplot2)
library(tidyverse)
tmp <- jsonlite::fromJSON("demos/accidents.json")
accident_data <- tmp$features$attributes |>
  mutate(accidentdate = ymd("1970-1-1") + seconds(accidentdate)/1000,
         date = floor_date(accidentdate, "days")) |> 
  group_by(date) |> count() 

all_days <- tibble(
  date = seq.Date(min(accident_data$date), 
                  max(accident_data$date)))


accident_data <- full_join(accident_data, all_days) 
                           
accident_data |>
  mutate(n = if_else(is.na(n), 0, n)) |>
  group_by(n) |>
  summarize(count = n()) |>
  mutate(prob = count/sum(count))


# get data by wget
# https://maps.loganutah.org/server/rest/services/Hosted/TrafficAccidentsHosted/FeatureServer/1/query?where=1%3D1&outFields=*&returnGeometry=false&f=json