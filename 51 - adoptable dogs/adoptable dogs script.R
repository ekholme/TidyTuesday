#Analyzing adoptable dogs data

library(tidyverse)
library(here)
library(wesanderson)
library(extrafont)
set.seed(0408)

wes_pal <- wes_palette("Darjeeling1")

dog_descrips<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

name_breed_counts <- dog_descrips %>%
  mutate(name = str_replace_all(name, "BELLA", "Bella")) %>%
  count(breed_primary, name, , name = "breed_name_n", sort = TRUE) %>%
  right_join(dog_descrips %>%
              count(breed_primary, name = "breed_n", sort = TRUE) %>%
               top_n(3),
             by = "breed_primary") %>%
  filter(breed_name_n > 10) %>%
  select(-breed_n) %>%
  mutate(breed_primary = as_factor(breed_primary))

#adding in some NA rows per each breed to put spacing between groups
empty_bars <- 4

empty <- tibble(
  breed_primary = rep(levels(name_breed_counts$breed_primary), each = empty_bars),
  name = NA,
  breed_name_n = NA
)

df <- bind_rows(name_breed_counts, empty) %>%
  arrange(breed_primary) %>%
  mutate(id = row_number(),
         breed_primary = as_factor(breed_primary)) %>%
  select(id, breed_primary, name, breed_name_n) %>%
  mutate(tot_row = nrow(.),
         angle = 90-360*(id-.5)/tot_row,
         hjust = if_else(angle < -90, 1, 0),
         use_angle = if_else(angle < -90, angle + 180, angle))

ggplot(df, aes(as_factor(id), y = breed_name_n, fill = breed_primary)) +
  geom_col(alpha = .7) +
  theme_void() +
  ylim(-10, 47) +
  coord_polar() +
  geom_text(aes(x = id, y = breed_name_n + 1, label = name, hjust = 0, angle = angle), color = "black", size = 3, alpha = .9, 
            family = "Rockwell") +
  scale_fill_manual(
    values = c(wes_pal[[1]], wes_pal[[3]], wes_pal[[4]]),
    name = NULL,
    labels = distinct(df, breed_primary)
  ) +
  annotate(geom = "text", x = 0, y = 35, label = "Most Popular Names of Most Common Shelter Breeds", family = "Rockwell",
           size = 8) +
  annotate(geom = "text", x = 0, y = 30, label = "Bella is very popular for all breeds. Zues mostly just for pitties.", family = "Rockwell", size = 4,
           alpha = .8) +
  theme(
    text = element_text(family = "Rockwell"),
    panel.background = element_rect(fill = wes_pal[[5]]),
    plot.background = element_rect(fill = wes_pal[[5]]),
    legend.position = c(.75, .25),
    panel.grid = element_blank(),
    plot.margin = grid::unit(c(-25, -25, -25, -25), "mm")
  )

ggsave(here("51 - adoptable dogs", "dog_names.jpeg"), device = "jpeg")
