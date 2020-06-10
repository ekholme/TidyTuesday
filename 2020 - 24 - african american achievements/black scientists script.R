

# Loading Packages & Data; Setting Constants ------------------------------

library(tidyverse)
library(ggtext)
library(extrafont)


science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

bckgrnd <- "#000000"
text_col <- "#FFFFFF"
family <- "Segoe UI Black"

# Cleaning Up Data --------------------------------------------------------


science_clean <- science %>%
  filter(!is.na(birth)) %>%
  arrange(birth) %>%
  mutate(death = replace_na(death, 2020),
         proper_name = if_else(str_detect(name, "Jr."),
                               str_replace_all(name, "^(.*), (.*), (Jr.)$", "\\2 \\1 \\3"),
                               str_replace_all(name, "^(.*), (.*)", "\\2 \\1")),
         id = row_number(),
         life = death - birth)

pad_vals <- c((nrow(science_clean)+1):(2*nrow(science_clean)))

science_padded <- science_clean %>%
  add_row(id = pad_vals) %>%
  mutate(tot_row = nrow(.),
         angle = 360*(id-.5)/tot_row,
         hjust = if_else(angle <= 90, 1, 0),
         use_angle = if_else(angle <= 90, 0 - angle, 180 - angle))


# Creating Plot -----------------------------------------------------------

ggplot(science_padded) +
  geom_segment(aes(x = id, xend = id, y = 0, yend = log(life)), color = text_col) +
  geom_text(aes(x = id, y = log(life) + .1, label = proper_name, hjust = hjust, angle = use_angle), color = "white", size = 2, family = family) +
  coord_polar(start = (3*pi)/2) +
  annotate(geom = "text", x = .75*nrow(science_padded), y = 1, label = "Data: Wikipedia | Viz: Eric Ekholm (@ekholm_e)", size = 2, color = text_col, family = family, fontface = "italic") +
  theme_void() +
  labs(
    title = "Notable Black Scientists"
  ) +
  theme(
    panel.background = element_rect(fill = bckgrnd),
    plot.background = element_rect(fill = bckgrnd),
    plot.margin = margin(t = 128, b = -200),
    plot.title = element_markdown(color = text_col, hjust = .5, family = family, size = 24)
  )

ggsave(here::here("2020 - 24 - african american achievements/scientists.png"), device = "png", units = "in")
