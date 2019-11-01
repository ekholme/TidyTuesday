
library(tidyverse)
library(ggmap)
library(ggimage)
library(extrafont)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

cp_6 <- get_map("Central Park, New York, NY", zoom = 14, maptype = "toner-lines", source = "stamen", color = "bw")

cp_map <- ggmap(cp_6)

#let's go with cp_6 for now!
squirrelz <- nyc_squirrels %>%
  mutate(dog_near = if_else(
    str_detect(other_activities, "dog") | str_detect(other_interactions, "dog"), "Y", "N"
  )) %>%
  filter(dog_near == "Y") %>%
  mutate(image = "https://image.flaticon.com/icons/png/512/91/91544.png")
  
  
x_start <- c(-73.992)
y_start <- c(40.802)

cp_map +
  geom_image(data = squirrelz,
             aes(x = long, y = lat, image = image), color = "red", size = .035) +
  labs(
    caption = "Data: NYC Squirrel Census | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  annotate("text", x = x_start, y = y_start, hjust = 0, parse = T,
           label = '"Squirrels in " * phantom("DANGER!")', color = "black", size = 8) +
  annotate("text", x = x_start, y = y_start, hjust = 0, parse = T,
           label = 'phantom("Squirrels in ") * bold("DANGER!")', color = "red", size = 8) +
  annotate("text", x = x_start, y = y_start - .0013, hjust = 0, size = 4,
           label = "Squirrels spotted near dogs.") +
  annotate("text", x = x_start, y = y_start - .0022, hjust = 0, size = 4,
           label = "Stay safe little friends!") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f2eadf"),
    text = element_text(family = "Rockwell"),
    panel.border = element_rect(color = "grey55", fill = NA)
  )
