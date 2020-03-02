
library(tidyverse)
library(ggtext)
library(USAboundaries)
library(sf)
library(patchwork)
library(nationalparkcolors)
library(extrafont)

fonttable()

pal <- park_palette("Denali")

#text_col <- pal[3]
background <- pal[2]
fill_col <- "#ed1944"
line_col <- pal[1]

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

county_df <- us_counties(resolution = "high") %>%
  rename(state = state_name,
         county = name)

measles_sum <- measles %>%
  filter(mmr > 0) %>%
  group_by(state, county) %>%
  summarize(mmr = mean(mmr, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(min_county = if_else(is.element(mmr, min(mmr)), "Y", NA_character_)) %>%
  ungroup() %>%
  left_join(x = county_df %>%
                  filter(is.element(state, .$state),
                         str_detect(state, "Alaska|Haw|Island|Puerto|Samoa|Guam|District", negate = TRUE)),
            y = .,
            by = c("state", "county"))

use_states <- measles_sum %>%
  filter(min_county == "Y") %>%
  distinct(state, .keep_all = TRUE) %>%
  mutate(title = paste0("<span style='font-size:16pt'>", state, "</span><br>", "<span style='font-size:11pt'>", county, ": ", round(mmr, digits = 1), "%</span>")) %>%
  select(state, title)

states <- use_states$state
titles <- use_states$title
  

p <- tibble(
  state = states,
  plots = purrr::map2(states, titles,
                function(x, y) {
                  ggplot() +
                    geom_sf(data = measles_sum %>%
                              filter(state == x),
                            aes(fill = min_county), color = line_col) +
                    theme_void() +
                    labs(
                      title = y
                    ) +
                    coord_sf() +
                    scale_fill_manual(
                      values = c("Y" = fill_col),
                      na.value = background
                    ) +
                    theme(
                      line = element_blank(),
                      text = element_text(family = "Bahnschrift"),
                      legend.position = "none",
                      plot.title = element_markdown(hjust = .5)
                    )
                })
) %>%
  arrange(state)

wrap_plots(p$plots) +
  plot_annotation(title = "Which Counties have the <span style='color:#ed1944'><b>Lowest</b></span> MMR Vaccination Rates?",
                  subtitle = "Data shows the average measles, mumps, and rubella (mmr) vaccination rate for schools in 18 states<br>during the 2017-2018 or 2018-2019 school year",
                  caption = "Data: The Wall Street Journal | Viz: Eric Ekholm (@ekholm_e)",
                  theme = theme(
                    text = element_text(family = "Bahnschrift"),
                    plot.title = element_markdown(size = 22, hjust = .5),
                    plot.subtitle = element_markdown(size = 12, hjust = .5, face = "italic"),
                    plot.margin = margin(.5, .5, .5, .5, unit = "cm")
                  ))

ggsave(here::here("2020 - 9 - measles/measles_plot.jpeg"), device = "jpeg", width = 10, height = 8.5)
