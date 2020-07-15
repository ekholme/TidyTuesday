
library(tidyverse)
library(hrbrthemes)
library(ggtext)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

green <- "#00ff00"

astronauts %>%
  mutate(num_aliens = 0) %>%
  count(year_of_mission, wt = num_aliens, name = "num_aliens") %>%
  ggplot(aes(x = year_of_mission, y = num_aliens)) +
  geom_point(color = green) +
  geom_line(color = green) +
  scale_y_continuous(
    limits = c(0, 10)
  ) +
  labs(
    x = "Year",
    y = "Number of Alien Encounters",
    title = "Number of Confirmed <span style='color:#00ff00'>Alien</span> Encounters by Astronauts over Time",
    caption = "Data: Mariya Stavnichuk & Tatsuya Corlett | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme_ft_rc() +
  theme(
    plot.title = element_markdown()
  )

ggsave(here::here("2020 - 29 - astronauts/aliens.png"), device = "png")