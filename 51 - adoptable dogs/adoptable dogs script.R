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
  count(breed_primary, name, , name = "breed_name_n", sort = TRUE) %>%
  right_join(dog_descrips %>%
              count(breed_primary, name = "breed_n", sort = TRUE) %>%
               top_n(3),
             by = "breed_primary") %>%
  filter(breed_name_n > 5) %>%
  select(-breed_n) %>%
  mutate(breed_primary = as_factor(breed_primary))

#adding in some NA rows per each breed to put spacing between groups
empty_bars <- 4
empty <- data.frame(matrix(NA, empty_bars*nlevels(name_breed_counts$breed_primary), ncol(name_breed_counts)))
colnames(empty) <- colnames(name_breed_counts)
empty$breed_primary <- rep(levels(name_breed_counts$breed_primary), each = empty_bars)

df <- bind_rows(name_breed_counts, empty) %>%
  arrange(breed_primary) %>%
  mutate(id = row_number(),
         breed_primary = as_factor(breed_primary)) %>%
  select(id, breed_primary, name, breed_name_n)
#RESUME HERE TOMORROW

#check out this code for help: https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html