
library(tidyverse)
library(broom)
library(tidytext)
library(glue)
library(eemisc)

theme_set(theme_eedark())

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')


# Data Setup --------------------------------------------------------------


main_cast <- friends %>%
  count(speaker) %>%
  slice_max(n = 6, order_by = n) %>%
  pull(speaker)

main_firsts <- str_remove_all(main_cast, " .*$")

combos <- t(combn(main_firsts, 2)) %>%
  as_tibble() %>%
  mutate(comb = glue("{ V1 } & { V2 }"),
         comb_inv = glue("{ V2 } & { V1 }"))

replace_comb <- combos$comb
names(replace_comb) <- combos$comb_inv

friends_exchanges <- friends %>%
  mutate(season_ep = paste0(season, ".", episode)) %>%
  group_by(season_ep) %>%
  mutate(char2 = lead(speaker)) %>%
  ungroup() %>%
  filter(speaker %in% main_cast & char2 %in% main_cast & speaker != char2) %>%
  mutate(across(c("speaker", "char2"), ~str_remove_all(., " .*$"))) %>%
  mutate(exchange = glue("{ speaker } & { char2 }") %>%
           str_replace_all(replace_comb)) %>%
  count(season_ep, exchange) %>%
  pivot_wider(names_from = exchange,
              values_from = n,
              values_fill = list(n = 0))


# Clustering --------------------------------------------------------------

set.seed(0408)
clusters_fit <- tibble(
  k = c(1:10),
  km_fit = map(c(1:10), ~kmeans(friends_exchanges %>% select(-season_ep), centers = .))
) %>%
  mutate(within_ss = map_dbl(km_fit, ~pluck(., 5)))

#looking for elbow in within sum of squares
clusters_fit %>%
  ggplot(aes(x = k, y = within_ss)) +
  geom_point(color = "white") +
  geom_line(color = "white")
#7 seems reasonable

fin_k <- 7


# Types Plot --------------------------------------------------------------

friends_clustered <- broom::augment(clusters_fit$km_fit[[fin_k]], data = friends_exchanges)

clusters_long <- friends_clustered %>%
  group_by(.cluster) %>%
  summarize(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  pivot_longer(cols = -c(".cluster"),
               names_to = "exchange",
               values_to = "lines")

clusters_long %>%
  group_by(.cluster) %>%
  slice_max(n = 5, order_by = lines) %>%
  ungroup() %>%
  mutate(.cluster = glue("Type { .cluster }")) %>%
  ggplot(aes(x = lines, y = reorder_within(exchange, lines, .cluster), fill = .cluster)) +
  geom_col() +
  facet_wrap(~.cluster, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_viridis_d() +
  labs(
    x = "Average Number of Exchanges",
    y = NULL,
    title = "Types of Friends Episodes",
    subtitle = "This plot shows the results of k-means clustering used to estimate the types of episodes based on the number of exchanges between characters.",
    caption = "Viz: Eric Ekholm (@ekholm_e) | Data: {friends} R package"
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown(size = 10.5)
  )

ggsave(here::here("2020 - 37 - friends/friend_types.png"), device = "png")
