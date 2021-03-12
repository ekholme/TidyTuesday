
library(tidyverse)
library(eemisc)
library(harrypotter)
library(ggbeeswarm)

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

movies <- movies %>%
  mutate(decade = year - (year %% 10))

luna <- hp(n = 2, option = "LunaLovegood")

p <- movies %>%
  ggplot(aes(x = as_factor(decade), y = imdb_rating, color = binary, group = as_factor(binary))) +
  geom_quasirandom(dodge.width = .7, width = .15, alpha = .6) +
  scale_color_hp_d(option = "LunaLovegood") +
  labs(
    y = "IMDB Rating",
    x = "Decade",
    title = "Movies that <span style='color:#830042'>Pass</span> and <span style='color:#084d49'>Fail</span> the Bechdel Test Have Similar IMDB Rating Distributions",
    #subtitle = "by decade",
    caption = "Data: FiveThirtyEight & IMDB | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme_ee(size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(here::here("2021 - 11 - bechdel test/bechdel.png"), p, device = "png")
