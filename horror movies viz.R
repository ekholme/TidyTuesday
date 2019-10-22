#horror movies viz

library(tidyverse)
library(tidytext)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#a bit of cleaning
horror <- horror_movies %>%
  extract(col = title, into = c("title", "year"), regex = "(.*) \\((\\d+)\\)") %>%
  mutate(year = as.integer(year)) %>%
  mutate(id = row_number()) %>%
  select(id, everything())

#let's try for most common words in titles? can maybe look at over time? or do sentiment analysis?
df <- horror %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>% #i like this -- goes beyond positive and negative
  group_by(sentiment) %>%
  mutate(sent_count = n()) %>%
  ungroup() %>%
  filter(str_detect(sentiment, "positive|negative", negate = TRUE))

##note -- can maybe add the emoji faces to correspond with sentiments?
#will probably want to filter out the 'positive' and 'negative' sentiments