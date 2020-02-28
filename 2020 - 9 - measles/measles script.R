
library(tidyverse)


measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

measles_sum <- measles %>%
  group_by(state) %>%
  summarize(mmr = mean(mmr, na.rm = TRUE)) %>%
  arrange(mmr) %>%
  filter(mmr > 0)
