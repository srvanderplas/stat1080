library(xkcd)
library(ggplot2)
library(dplyr)
library(tibble)

tibble(x = rchisq(100, df=8)) |>
  mutate(xbin = cut_interval(x, length = .5, boundary = 0),
         xmin = str_extract(as.character(xbin), ".([\\d\\.?]{1,}?),([\\d\\.?]{1,}).", group = 1) |> as.numeric(),
         xmax = str_extract(as.character(xbin), ".([\\d\\.?]{1,}?),([\\d\\.?]{1,}).", group = 2) |> as.numeric()) |>
  group_by(xbin, xmin, xmax) |> 
  summarize(count = n()) |>
  ggplot() + xkcdrect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count), 
                      borderxjitteramount = 0.05,  wobble = 0.5) + 
  theme_xkcd()
