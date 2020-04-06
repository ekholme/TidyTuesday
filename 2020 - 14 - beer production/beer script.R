
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

library(tidyverse)
library(sf)
library(cartogram)
library(tidycensus)
library(albersusa)
library(ggthemes)
library(ggtext)
library(patchwork)
library(extrafont)

#specifying some colors
background <- "grey5"
text_col <- "white"
main_fill <- "#08f7fe"
facet_fills <- c("#09fbd3", "#fe53bb", "#f5d300")
ff <- "Berlin Sans FB"

pop <- get_acs(geograph = "state", table = "B01003") %>%
  select(state = NAME,
         pop = estimate)

total_beer <- beer_states %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(beer = sum(barrels, na.rm = TRUE)) %>%
  ungroup()

usa_beers <- usa_sf() %>%
  st_transform(3857) %>%
  inner_join(pop, by = c("name" = "state")) %>%
  inner_join(total_beer, by = c("iso_3166_2" = "state")) %>%
  mutate(beer_per_capita = beer/pop,
         log_bpc = log(beer_per_capita))

beers_cart <- cartogram_cont(usa_beers, "beer_per_capita")

top_labs <- beers_cart %>%
  top_n(n = 7, wt = log_bpc) %>%
  pull(name)

p1 <- beers_cart %>%
  ggplot() +
    geom_sf(aes(alpha = scale(log_bpc)), fill = main_fill) +
    geom_sf_text(aes(label = if_else(name %in% top_labs, str_replace(name, " ", "\n"), NA_character_
    )), color = "white", fontface = "bold", family = ff, size = 3) +
    theme_map() +
    labs(
      title = "Total Beer Production"
    ) +
    theme(
      text = element_text(color = "white", family = ff),
      rect = element_rect(fill = background, color = background),
      panel.background = element_rect(fill = background),
      plot.background = element_rect(fill = background),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = .5, size = 16),
      plot.margin = margin(0, 0, -35, 0)
    )


beers_cats <- usa_sf() %>%
  st_transform(3857) %>%
  inner_join(pop, by = c("name" = "state")) %>%
  inner_join(beer_states %>%
               filter(year == 2018), by = c("iso_3166_2" = "state")) %>%
  mutate(beer_per_capita = barrels/pop,
         log_bpc = log(beer_per_capita))

beers_cat_map <- beers_cats %>%
  group_by(type) %>%
  group_map(~cartogram_cont(.x, "beer_per_capita"), keep = TRUE)


beers_rescaled <- bind_rows(beers_cat_map) %>%
  group_by(type) %>%
  mutate(alpha = scale(log_bpc))


p2 <- beers_rescaled %>%
  ggplot() +
  geom_sf(aes(alpha = alpha, fill = type)) + 
  facet_wrap(~type, ncol = 3) +
  scale_fill_manual(
    values = facet_fills
  ) +
  theme_map() +
  theme(
    rect = element_rect(fill = background, color = background),
    panel.background = element_rect(fill = background),
    plot.background = element_rect(fill = background),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.text = element_markdown(
      color = "white", family = ff, size = 14
    ),
    strip.background = element_blank(),
    plot.margin = margin(0, 0, -75, 0)
  )

patch <- p1 / p2 +
  plot_annotation(
    title = "Which States Produce the Most Beer\nper Capita?",
    subtitle = "State size and color scaled to represent the amount of beer produced per person living in that state",
    caption = "Data: Alcohol and Tobacco Tax & Trade Bureau | Viz: Eric Ekholm (@ekholm_e)",
    theme = theme(
      plot.title = element_markdown(
        size = 18, hjust = .5
      ),
      plot.subtitle = element_markdown(face = "italic")
    )
  ) &
  theme(
    plot.background = element_rect(fill = background, color = background),
    text = element_text(color = "white", family = ff),
    plot.caption = element_text(hjust = .5),
    plot.subtitle = element_markdown(face = "italic")
  )

ggsave(here::here("2020 - 14 - beer production/beer_maps.pdf"), patch, device = "pdf")
