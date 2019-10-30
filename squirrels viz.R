#viz for squirrels TidyTuesday -- week of 10/29/19

library(tidyverse)
library(leaflet)
library(tidytext)
library(wordcloud2)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

squirrelz <- nyc_squirrels %>%
  extract(date, into = c("month", "day", "year"), regex = "(\\d{2})(\\d{2})(\\d{4})") %>%
  mutate_at(.vars = c("month", "day", "year"), ~as.integer(.))

squirrelz %>% 
  leaflet() %>% 
  addTiles() %>%
  addMarkers(lng = squirrelz$long, lat = squirrelz$lat)
#ok, this works -- not sure what to do with it

acts <- distinct(squirrelz, other_activities) %>% as_vector()
locs <- distinct(squirrelz, specific_location) %>% as_vector()

#let's try a fun word cloud in the outline of a squirrel for either custom squirrel activities
#or custom squirrel locations
#possible to map the color aesthetic onto something?

##new idea -- maybe plop this guy on top of a bar chart with counts of squirrels by color -- and color the text?

#proof of concept squirrel outline
test <- squirrelz %>%
  select(primary_fur_color, other_activities) %>%
  unnest_tokens(word, other_activities) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word)) %>%
  group_by(word) %>%
  summarize(freq = n()) %>%
  ungroup()

wordcloud2(data = test, figPath = "squirrel_out.png",
           size = .25)

wordcloud2(data = test, shape = 'star', color = "steelblue")
##try more tomorrow with ggwordcloud function -- this is starting to get really frustrating, though
#https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

library(ggwordcloud)
#> Loading required package: ggplot2
data("love_words_small")