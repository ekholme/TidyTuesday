

# Setup -------------------------------------------------------------------


energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

library(tidyverse)
library(ggrepel)
library(ggtext)
library(ragg)

theme_set(theme_minimal())

fam <- "Roboto Condensed"

# Data Setup --------------------------------------------------------------

types <- energy_types %>%
  select(-c("2016", "2017")) %>% #limiting to 2018
  filter(type != "Other") %>% #removing bc I don't know what this is
  mutate(en_type = case_when(
    type == "Conventional thermal" ~ type,
    type == "Nuclear" ~ type,
    TRUE ~ "Renewable"
  )) %>%
  group_by(en_type, country, country_name) %>%
  summarize(amount = sum(`2018`)) %>%
  ungroup() %>%
  add_count(country, wt = amount, name = "total") %>%
  mutate(perc = amount/total) %>%
  arrange(country, en_type)

ord_tbl <- types %>%
  filter(en_type == "Conventional thermal") %>%
  arrange(perc) %>%
  mutate(order = row_number()) %>%
  select(country, order)

df <- types %>%
  left_join(ord_tbl, by = "country") %>%
  distinct(country, total, order) %>%
  arrange(order) %>%
  mutate(cumul = cumsum(total),
         x_beg = lag(cumul, default = 0),
         x_end = x_beg + total,
         lab_x_pos = (x_beg + x_end)/2) %>%
  select(-total) %>%
  left_join(x = types,
            y = .,
            by = "country") %>%
  mutate(renew_y_beg = lag(perc)) %>%
  arrange(order)

left_labs <- tibble(
  x = -100000,
  y = c(0, -.25, -.5, -.75, -1),
  label = c("0%", "25%", "50%", "75%", "100% conventional thermal energy")
)

right_labs <- tibble(
  x = max(df$cumul + 200000),
  y = c(0, .25, .5, .75, 1),
  label = c("0%", "25%", "50%", "75%", "clean energy 100%")
)


# Plotting ----------------------------------------------------------------


file <- here::here("2020 - 32 - euro energy/energy.png")
size <- 6
bckgrnd <- "white"

agg_png(file, width = 1200, height = 900, units = "px")

ggplot() +
  geom_rect(data = df %>%
              filter(en_type == "Conventional thermal"),
            aes(xmin = x_beg, xmax = x_end, ymin = 0, ymax = -1*perc), alpha = .7, fill = "grey", color = NA) +
  geom_rect(data = df %>%
              filter(en_type == "Nuclear"),
            aes(xmin = x_beg, xmax = x_end, ymin = 0, ymax = perc), alpha = .7, fill = "yellow", color = NA) +
  geom_rect(data = df %>%
              filter(en_type == "Renewable"),
            aes(xmin = x_beg, xmax = x_end, ymin = renew_y_beg, ymax = perc + renew_y_beg), alpha = .7, 
            fill = "orange", color = NA) +
  geom_text_repel(data = df %>%
              filter(en_type == "Renewable"),
            aes(x = lab_x_pos, y = perc + renew_y_beg + .025, label = country), size = size, direction = "y",
            segment.color = NA, force = .04, box.padding = 0, point.padding = 0, family = fam) +
  annotate("richtext", x = df %>% pluck(11, 7), y = 1.2,
           label = "**Norway** had an electricity production<br>almost entirely made up of renewable<br>energy (97%). It is the second largest<br> producer of this type of energy in Europe.",
           family = fam, hjust = 0, size = size, label.color = NA, fill = NA) +
  annotate("curve", x = pluck(df, 11, 4), xend = pluck(df, 11, 7), y = 1.02, yend = 1.2, curvature = -.4) +
  annotate("richtext", x = pluck(df, 10, 7), y = -.6,
           label = "**France** is the second largest energy producer<br>in Europe but by far the largest nuclear energy<br>provider representing 71% of its production.",
           family = fam, hjust = 0, size = size, label.color = NA, fill = NA) +
  annotate("segment", x = pluck(df, 11, 11), xend = pluck(df, 11, 11), y = -.12, yend = -.5) +
  annotate("richtext", x = pluck(df, 10, 68) - 1e5, y = .75,
           label = "**Germany** is the largest energy producer in<br>Europe and also produced the most renewable<br>and conventional thermal energy (representing<br>31% and 56% of its production, respectively).",
           family = fam, hjust = 0, size = size, label.color = NA, fill = NA) +
  annotate("curve", x = pluck(df, 11, 68), xend = pluck(df, 10, 68) - 1e5, y = .51, yend = .75, curvature = -.4) +
  annotate("richtext", x = pluck(df, 11, 87), y = -.9,
           label = "Most of **Poland's** electricity production is<br>from conventional thermal energy (90%).",
           family = fam, hjust = 0.5, size = size, label.color = NA, fill = NA) +
  annotate("curve", x = pluck(df, 11, 101), xend = pluck(df, 11, 98), y = -.92, yend = -.92, curvature = -.3) +
  geom_hline(yintercept = 1.25, color = bckgrnd) +
  geom_text(data = left_labs, aes(x = x, y = y, label = label), hjust = 0, size = size) +
  geom_text(data = right_labs, aes(x = x, y = y, label = label), hjust = 1, size = size) +
  annotate("rect", xmin = pluck(df, 11, 86), xmax = pluck(df, 11, 89) - 1e5, ymin = 1.23, ymax = 1.3, fill = "orange", alpha = .7) +
  annotate("rect", xmin = pluck(df, 11, 86), xmax = pluck(df, 11, 89) - 1e5, ymin = 1.16, ymax = 1.23, fill = "yellow", alpha = .7) +
  annotate("rect", xmin = pluck(df, 11, 86), xmax = pluck(df, 11, 89) - 1e5, ymin = 1.02, ymax = 1.16, fill = "grey", alpha = .7) +
  annotate("richtext", x = pluck(df, 11, 89) - 90000, y = 1.265, label = "Renewable", size = 4.5, 
           family = fam, hjust = 0, size = size, label.color = NA, fill = NA) +
  annotate("richtext", x = pluck(df, 11, 89) - 90000, y = 1.195, label = "Nuclear", size = 4.5, 
           family = fam, hjust = 0, size = size, label.color = NA, fill = NA) +
  annotate("richtext", x = pluck(df, 11, 86) - 10000, y = 1.09, label = "Conventional<br>thermal", size = 4.5, 
           family = fam, hjust = 1, vjust = .5, size = size, label.color = NA, fill = NA) +
  annotate("segment", x = pluck(df, 11, 86), xend = pluck(df, 11, 86), y = 1.01, yend = .99) +
  annotate("segment", x = pluck(df, 11, 89) - 1e5, xend = pluck(df, 11, 89) - 1e5, y = 1.01, yend = .99) +
  annotate("segment", x = pluck(df, 11, 86), xend = pluck(df, 11, 89) - 1e5, y = .99, yend = .99) +
  annotate("richtext", x = mean(c(pluck(df, 11, 86), pluck(df, 11, 89) - 1e5)), y = .93,
           label = "Total energy<br>produced", size = 4.5,
           family = fam, hjust = .5, size = size, label.color = NA, fill = NA) +
  labs(
    title = "How European countries generated electricity in 2018",
    x = NULL,
    y = NULL,
    caption = "Data: Eurostat | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  expand_limits(y = 1.3) +
  scale_x_continuous(limits = c(-100000, max(df$cumul) + 200000)) +
  theme(
    plot.background = element_rect(fill = bckgrnd),
    text = element_text(family = fam),
    plot.title = element_markdown(family = fam, face = "bold", size = 28),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.caption = element_markdown(family = fam, size = 14, face = "italic")
  )


invisible(dev.off())
