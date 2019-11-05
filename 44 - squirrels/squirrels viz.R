
library(tidyverse)
library(ggmap)
library(ggimage)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

cp_1 <- get_map("Central Park, New York, NY", zoom = 14, maptype = "roadmap", source = "google", color = "bw")
cp_2 <- get_map("Central Park, New York, NY", zoom = 13, maptype = "terrain", source = "google", color = "bw")
cp_3 <- get_map("Central Park, New York, NY", zoom = 14.5, maptype = "toner", source = "stamen", color = "bw")
cp_4 <- get_map("Central Park, New York, NY", zoom = 14, maptype = "satellite", color = "bw")

ggmap(cp_1)
ggmap(cp_2)
ggmap(cp_3)
ggmap(cp_4)

#let's stic with toner but zoom in a bit
pos <- c(min(nyc_squirrels$long), min(nyc_squirrels$lat), max(nyc_squirrels$long), max(nyc_squirrels$lat))

cp_5 <- get_map(pos, maptype = "toner-lines", source = "stamen", color = "bw", force = TRUE)
cp_6 <- get_map("Central Park, New York, NY", zoom = 14, maptype = "toner-lines", source = "stamen", color = "bw")

cp_map <- ggmap(cp_6)

#let's go with cp_6 for now!
squirrelz <- nyc_squirrels %>%
  mutate(dog_near = if_else(
    str_detect(other_activities, "dog") | str_detect(other_interactions, "dog"), "Y", "N"
  )) %>%
  filter(dog_near == "Y") %>%
  mutate(image = "https://image.flaticon.com/icons/png/512/91/91544.png")
  
  
x_start <- c(-73.984)
y_start <- c(40.802)

cp_map +
  geom_image(data = squirrelz,
             aes(x = long, y = lat, image = image), color = "red") +
  #geom_text()
  labs(
    #x = NULL,
    #y = NULL
  ) +
  annotate("text", x = x_start, y = y_start, hjust = 0, parse = T,
           label = '"Squirrels in " * phantom("DANGER!")', color = "black") +
  annotate("text", x = x_start, y = y_start, hjust = 0, parse = T,
           label = 'phantom("Squirrels in ") * "DANGER!"', color = "red") +
  annotate("text", x = x_start, y = y_start - .002, hjust = 0,
           label = "Squirrels spotted near dogs.") +
  annotate("text", x = x_start, y = y_start - .004, hjust = 0,
           label = "Stay safe little friends!") +
  theme_void() +
  theme(
    plot.background = element_rect(color = "lightslategray", fill = "lightslategray")
  ) #need to play around with text -- position, size, font, spacing, etc.
