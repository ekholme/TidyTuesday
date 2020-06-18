
library(tidyverse)
library(USAboundaries)
library(sf)
library(fuzzyjoin)
library(nord)
library(ggtext)
library(extrafont)

census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')

bckgrnd <- "grey20"
ratio <- 17.5/5
lum <- nord_palettes$lumina
family = 'Rockwell'


states_map <- us_states() %>%
  filter(str_detect(stusps, "DC|PR|HI|AK", negate = TRUE))

census_small <- census %>%
  filter(region != "USA Total",
          !is.na(division))
  
divs <- unique(census_small$division)


join_tbl <- tibble(
  division = c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central",
               "Mountain", "Pacific"),
  sts = c("CT|ME|MA|NH|RI|VT",
          "NJ|NY|PA",
          "IN|IL|OH|MI|WI",
          "ND|SD|NE|KS|MN|IA|MO",
          "DE|FL|DC|GA|MD|NC|SC|VA|WV",
          "AL|MS|TN|KY",
          "AR|LA|OK|TX",
          "AZ|CO|ID|NM|MT|UT|NV|WY",
          "AK|CA|OR|WA|HI")
)

st_map_joined <- states_map %>%
  regex_left_join(join_tbl, by = c("stusps" = "sts"))

census_use <- census_small %>%
  expand(year, division) %>%
  left_join(census_small) %>%
  left_join(st_map_joined, by = "division") %>%
  mutate(perc_free_blacks = black_free/black)


census_use %>%
  st_as_sf() %>%
  filter(year %in% c("1810", "1830", "1850", "1870")) %>%
  ggplot() +
  geom_sf(aes(fill = perc_free_blacks), size = .1) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_gradient(low = lum[[5]], high = lum[[1]],
                      name = "Percent of Free African Americans",
                      breaks = c(0, 1),
                      labels = c("0%", "100%")) +
  labs(
    title = "The Road to Freedom",
    subtitle = "This plot shows the percent of free African Americans in the United States between 1810 and 1870 by Census division. Data was not available for the Mountain and Pacific regions before 1850.<br>",
    caption = "Data: US Decennial Census | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    
    plot.background = element_rect(fill = bckgrnd),
    text = element_text(color = "white", family = family),
    plot.title = element_markdown(hjust = .5, size = 20),
    plot.subtitle = element_markdown(size = 6, hjust = .5, face = "italic"),
    plot.caption = element_markdown(size = 3),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 4),
    strip.text = element_markdown(size = 5)
  ) +
  guides(fill = guide_colorbar(barwidth = 6, barheight = .5, title.position = "top", title.hjust = .5))

ggsave(here::here("2020 - 25 - african american history/test.png"), device = "png", width = 20, height = 5.45, unit = "cm")


