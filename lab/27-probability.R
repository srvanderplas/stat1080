utah_reg_time <- utah_reg |>
  group_by(party) |>
  summarize(mean = mean(count, na.rm = T),
            sd = sd(count, na.rm = T),
            pct = (max(count)-min(count))/(mean(count))) |>
  ungroup() |>
  arrange(desc(pct))

head(utah_reg_time)


utah_party_sample <- utah_reg |> 
  filter(date == max(date)) |>
  mutate(prop = count/sum(count), 
         sample = 3000 * prop)


library(infer)
