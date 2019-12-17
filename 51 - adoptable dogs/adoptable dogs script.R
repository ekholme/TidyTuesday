#Analyzing adoptable dogs data

library(tidyverse)
library(here)
set.seed(0408)

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descrips<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

dog_descrips %>%
  count(breed_primary, sort = TRUE) %>%
  ggplot(aes(x = fct_reorder(breed_primary, n), y = n)) +
  geom_col() +
  coord_flip()

dog_descrips %>%
  ggplot(aes(age)) +
  geom_bar()

dog_descrips %>%
  ggplot(aes(sex)) +
  geom_bar()

dog_descrips %>%
  count(special_needs)
#we see roughly 56k without special needs and 2.1k with special needs

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
  select(id, breed_primary, name, breed_name_n)

df <- df %>%
  mutate(tot_row = nrow(df),
         angle = 90-360*(id-.5)/tot_row,
         hjust = if_else(angle < -90, 1, 0),
         use_angle = if_else(angle < -90, angle + 180, angle))

axis_labs <- df %>%
  group_by(breed_primary) %>%
  summarize(start = min(id),
            end = max(id) - empty_bars) %>%
  mutate(title = (start+end)/2) %>%
  ungroup()

ggplot(df, aes(as_factor(id), y = breed_name_n, fill = breed_primary)) +
  geom_col(alpha = .7) +
  theme_void() +
  ylim(-10, 47) +
  theme(
    plot.margin = unit(c(-1, -2, -1, -2), "cm"),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  coord_polar() +
  geom_text(aes(x = id, y = breed_name_n + 1, label = name, hjust = 0, angle = angle), color = "black", size = 2.5, alpha = .6) +
  #geom_segment(data = axis_labs, aes(x = start, y = -2, xend = end, yend = -2), color = "black")
  geom_text(data = axis_labs, aes(x = title, y = -2, label = breed_primary), color = "black") #still need to play around wtih text position
  #maybe makes sense to put it on the outside. Plus colors

#check out this code for help: https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html