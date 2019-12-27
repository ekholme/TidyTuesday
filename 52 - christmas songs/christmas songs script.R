library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(extrafont)

set.seed(0408)

christmas_lyrics <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv")

crosby_bigrams <- christmas_lyrics %>%
  unnest_tokens(bigram, lyric, token = "ngrams", n = 2) %>%
  filter(artist == "Bing Crosby") %>%
  separate(bigram, c("word1", "word2"), " ") %>%
  filter(!is.element(word1, stop_words$word) & 
           !is.element(word2, stop_words$word) & 
           !is.na(word1) & 
           !is.na(word2)
         ) %>%
  count(word1, word2, sort = TRUE) 

crosby_words <- christmas_lyrics %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  filter(artist == "Bing Crosby") %>%
  count(word, sort = TRUE) %>%
  mutate(n = log(n)) %>%
  filter(is.element(word, crosby_bigrams$word1) |
           is.element(word, crosby_bigrams$word2))

crosby_graph <- graph_from_data_frame(crosby_bigrams, vertices = crosby_words)

background_col <- "#8bbbce"
network_col <- "#dddddd"

cosby_network <- ggraph(crosby_graph, layout = "igraph", algorithm = "fr") +
  geom_edge_link(color = network_col) +
  geom_node_point(aes(size = n), color = network_col) +
  geom_node_text(aes(label = name, size = n), repel = TRUE, family = "Garamond") +
  theme_void() +
  labs(
    title = "Lyrics from Bing Crosby's White Christmas Album (1958)",
    subtitle = "This plot shows connections between words found in the White Christmas album, with point size and text size indicating word frequency.",
    caption = "Lyrics from the Genius package | Viz by Eric Ekholm (@ekholm_e)"
  ) +
  theme(
    text = element_text(family = "Garamond"),
    legend.position = "none",
    panel.background = element_rect(fill = background_col),
    plot.background = element_rect(fill = background_col),
    plot.title = element_text(size = 18, hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5, face = "italic")
  )

ggsave(here::here("52 - christmas songs/cosby_network.jpeg"), plot = cosby_network, device = "jpeg")
  