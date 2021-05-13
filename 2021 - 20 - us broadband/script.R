
library(tidyverse)
library(eemisc)
library(joycon)
library(janitor)

#pulling out the color I want to use
pal <- joycon_pal("hyrule", n = 5, type = "discrete")
col1 <- pal[4]
col2 <- pal[1]

broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

broadband <- broadband %>%
  clean_names() %>%
  mutate(across(4:5, as.numeric)) 

var_df <- broadband %>%
  group_by(st) %>%
  summarize(sd = sd(broadband_availability_per_fcc, na.rm = TRUE)) %>%
  filter(sd != 0)

p <- var_df %>%
  ggplot(aes(x = sd, y = fct_reorder(st, sd))) +
  geom_segment(aes(x = 0, xend = sd, y = fct_reorder(st, sd), yend = fct_reorder(st, sd)), color = col1) +
  geom_point(color = col2, size = 3.5) +
  scale_x_continuous(expand = expansion(add = c(0.001, .01))) +
  labs(
    x = "Standard Deviation of Broadband Availability",
    y = "State",
    title = "Inequality in Broadband Access by State",
    subtitle = "This plot shows the standard deviation of broadband availability, as indicated by the % of people in the county with access to broadband,<br>by state. States with larger standard deviations are more unequal in how accessible broadband is to citizens.",
    caption = "Data: Microsoft | Viz: Eric Ekholm (@ekholme_e)"
  ) +
  theme_eedark(size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(here::here("2021 - 20 - us broadband/broadband_sd.png"), p, device = "png", height = 8)
