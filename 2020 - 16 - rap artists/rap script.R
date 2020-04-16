
library(tidyverse)
library(hrbrthemes)
library(ggtext)

theme_set(theme_ipsum(grid = "Y"))

fills <- c("N" = "grey50", "Y" = "#C93312")

rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

yr_artist_pts <- rankings %>%
  filter(year >= 2000) %>%
  mutate(artist = str_remove(artist, " ft. .*")) %>%
  group_by(artist, year) %>%
  summarize(points = sum(points, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(is_max = if_else(points == max(points), "Y", "N") %>%
           as_factor()) %>%
  ungroup()

lab_df <- yr_artist_pts %>%
  group_by(year) %>%
  mutate(all_pts = sum(points)) %>%
  ungroup() %>%
  filter(is_max == "Y") %>%
  distinct(year, .keep_all = TRUE)

yr_artist_pts %>%
  ggplot(aes(x = as_factor(year), y = points, fill = fct_relevel(is_max, "Y", "N"))) +
  geom_col() +
  geom_text(data = lab_df,
            aes(x = as_factor(year), y = all_pts + 1, label = artist), angle = 90, hjust = 0, color = fills[2],
            fontface = "bold", size = 3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(
    limits = c(0, 150)
  ) +
  scale_fill_manual(
    values = fills
  ) +
  labs(
    x = "Year",
    y = "Points",
    title = "Is Hip Hop Just a Euphemism for a New Religion?",
    subtitle = "This plot shows the points earned by all rap songs released in a given year, according to a<br>BBC Music critics poll. The artist earning the most points in each year\nis highlighed in <span style='color:#C93312'>**red**</span>.<br>Kanye's *My Beautiful Dark Twisted Fantasy*, released in 2010, is the most acclaimed album since 2000.",
    caption = "Data: BBC Music | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.subtitle = element_markdown()
  )

ggsave(here::here("2020 - 16 - rap artists/rap_bars.png"), device = "png")
