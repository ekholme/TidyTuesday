
library(tidyverse)
library(ggchicklet)
library(eemisc)
library(ggtext)
library(ragg)

#theme_set(theme_ee())


# Cleaning ----------------------------------------------------------------


beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

va_breweries <- beer_awards %>%
  filter(state == "VA" & year >=2000) %>%
  mutate(brewery = case_when(
    str_detect(brewery, "Hardywood") ~ "Hardywood",
    str_detect(brewery, "Devils Backbone") ~ "Devils Backbone",
    TRUE ~ brewery
  ))

va_year_medals <- va_breweries %>%
  count(year, medal) %>%
  add_count(year, wt = n, name = "total_medals")


# Plotting ----------------------------------------------------------------



line_col <- "grey80"
medal_cols <- c("#cd7f32", "#c0c0c0", "#ffd700")
bckgrnd <- '#f4e7c5'
dots <- '#f6e7d1'
txt <- "Lobster"
size <- 12

file <- here::here("2020 - 43 - great american beer fest/va_beers.png")

agg_png(file, width = 1200, height = 900, units = "px")

va_year_medals %>%
  ggplot(aes(x = year, y = total_medals)) +
  geom_line(color = line_col) +
  geom_point(color = line_col, fill = dots, shape = 21, size = 8,) +
  geom_chicklet(aes(x = year, y = n, fill = fct_relevel(medal, "Bronze", "Silver", "Gold")), color = bckgrnd, width = .4, show.legend = FALSE) +
  labs(
    y = "Total Medals",
    x = NULL,
    title = "Good Beer in Virginia",
    subtitle = "This plot shows the medals won by Virginia breweries at the Great American Beer Festival in the 21st century",
    caption = "Data: Great American Beer Festival | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  scale_fill_manual(
    values = medal_cols
  ) +
  theme(
    text = element_text(family = txt),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = bckgrnd),
    panel.background = element_rect(fill = bckgrnd),
    axis.ticks = element_blank(),
    plot.subtitle = element_markdown(size = 20),
    plot.title = element_text(family = txt, size = 34),
    plot.caption = element_markdown(family = txt, size = 14),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    plot.margin = margin(t = 3*size, b = 3*size, l = 1.5*size, r = 1.5*size)
  )

invisible(dev.off())