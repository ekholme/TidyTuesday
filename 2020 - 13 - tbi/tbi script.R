
library(tidyverse)


tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

#quick plan is to use motor vehicle crashes over time, fit a smoother, and make this look like a road

tbi_cars <- tbi_year %>%
  filter(injury_mechanism == "Motor vehicle crashes") %>%
  group_by(year) %>%
  summarize(n = sum(number_est)) %>%
  ungroup()
  
xx <- loess(n ~ year, data = tbi_cars)
  
  ggplot(aes(x = year, y = n)) +
    geom_line() +
    geom_smooth(lty = 2, color = "red")
