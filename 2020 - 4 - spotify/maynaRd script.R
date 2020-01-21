set.seed(0408)

library(tidyverse)
library(spotifyr)
library(genius)
library(hrbrthemes)
library(ggtext)

import_roboto_condensed()

theme_set(theme_ft_rc())

bands <- c("tool", "a perfect circle", "puscifer")

maynard_raw <- bands %>%
  map(~get_discography(artist = .)) %>%
  reduce(bind_rows)

#it looks like we have some duplicate tracks plus some remixes, so let's get rid of those
maynard <- maynard_raw %>%
  ungroup() %>%
  mutate_if(is.character, ~str_to_lower(.)) %>% #making everything lowercase so I don't have to deal with capitalization
  filter(str_detect(track_name, "mix", negate = TRUE),
         str_detect(album_name, "mix|load|amotion", negate = TRUE)) %>%
  distinct(track_name, .keep_all = TRUE) %>%
  mutate(duration_s = duration_ms/1000,
         artist_name = str_replace_all(artist_name, c("tool" = "Tool", "a perfect circle" = "APC", "puscifer" = "Puscifer")))

maynard_plot <- maynard %>%
  distinct(artist_name, album_name) %>%
  group_by(artist_name) %>%
  mutate(shading = rev(row_number())) %>%
  select(artist_name, album_name, shading) %>%
  left_join(x = maynard, y = ., by = c("artist_name", "album_name")) %>%
  arrange(track_name) %>%
  mutate(id = row_number())

#defining some values that will be helpful in the plot
pad <- 1000
line_color <- "#464950"
bckgrnd <- "#252a32"

#getting the 5 longest songs to add as labels
labels <- maynard_plot %>%
  top_n(n = 5, wt = duration_s) %>%
  mutate(track_name = str_to_title(track_name),
         hjust = if_else(id > median(id), 1, 0))

#setting up text to go in the middle of the circle
center_text <- tibble(
  labelz = "**<p style='color:white;font-size:11pt'>Duration of songs by Maynard James Keenan's bands: <span style='color:#ff0055'>Tool</span>, <span style='color:#909495'>A Perfect Circle</span>, and <span style='color:#0b53c1'>Puscifer.</span><br><br><span style='font-size:08pt'>*The 5 longest songs are all by <span style='color:#ff0055'>Tool.</span> The three rings correspond to 1 min, 5 mins, and 10 mins.</span>*</p>**",
  x = 0,
  y2 = -170
)

mjk_circle <- ggplot(maynard_plot, aes(id, duration_s, color = artist_name)) +
  geom_hline(aes(yintercept = (60)), color = line_color) +
  geom_hline(aes(yintercept = (5*60)), color = line_color) +
  geom_hline(aes(yintercept = (10*60)), color = line_color) +
  geom_segment(aes(x = id, xend = id, y = 0, yend = duration_s)) +
  geom_point() +
  geom_richtext(data = labels,
                aes(x = id, y = duration_s + 50, color = artist_name, label = track_name, hjust = hjust),
                size = 3,
                family = "Roboto Condensed",
                label.color = NA,
                fill = NA
                ) +
  geom_textbox(data = center_text,
               aes(x = x, y = y2, label = labelz),
               fill = NA, color = NA,
               width = unit(45, "mm"),
               box.hjust = .5,
               family = "Roboto Condensed",
               size = 3,
               hjust = .5) +
  coord_polar() +
  ylim(-.5*pad, max(maynard_plot$duration_s) + 850) +
  scale_color_ft() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = bckgrnd),
    plot.background = element_rect(fill = bckgrnd),
    legend.position = "none",
    plot.margin = margin(-15, -15, -15, -15, "lines")
  )

ggsave(here::here("2020 - 4 - spotify/mjk_circle.jpg"), plot = mjk_circle, device = "jpeg")
