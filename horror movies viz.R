#horror movies viz

library(tidyverse)
library(tidytext)
library(gganimate)
library(extrafont)

loadfonts(device = "win")

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#a bit of cleaning
horror <- horror_movies %>%
  extract(col = title, into = c("title", "year"), regex = "(.*) \\((\\d+)\\)") %>%
  mutate(year = as.integer(year)) %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>% #i like the nrc sentiment analysis for this -- goes beyond positive and negative
  filter(str_detect(sentiment, "positive|negative", negate = TRUE)) %>%
  group_by(sentiment, year) %>%
  mutate(sent_count_year = n()) %>%
  ungroup() %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  mutate(sum_year = n()) %>%
  mutate(prop_sent_count_year = sent_count_year/sum_year,
         sentiment = tools::toTitleCase(sentiment)) %>%
  arrange(year)

#plan is to count sentiments over time
plot <- ggplot(horror, aes(x = year, y = prop_sent_count_year, 
                           group = sentiment, color = sentiment)) +
  geom_line(size = 1) +
  #geom_segment(aes(xend = 2017, yend = prop_sent_count_year), color = "ivory", linetype = "dashed") +
  geom_text(aes(x = year, label = sentiment), fontface = "bold", size = 8, family = "Chiller") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Percent of Titles",
    title = "Which Sentiments Do Horror Movie Titles Elicit?",
    subtitle = paste0("From 'The Exorcist' to 'It,' horror movie\ titles can vary greatly. This graph shows\nthe sentiments associated with words in horror movie titles over time."),
    caption = "Data: Georgios Karamanis | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  scale_color_brewer(
    type = "seq",
    palette = "YlOrRd"
  ) +
  scale_x_continuous(
    limits = c(2012, 2018),
    expand = c(0, 0)
  ) +
  theme(
    plot.background = element_rect(fill = "grey5"),
    text = element_text(color = "ivory", family = "Chiller"),
    axis.text = element_text(color = "ivory", size = 18),
    axis.line = element_line(color = "ivory"),
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid = element_line(size = .05, color = "grey90"),
    plot.margin = unit(c(1, 2, 1, 2), "cm"),
    axis.title = element_text(size = 22),
    plot.title = element_text(size = 30, hjust = 0),
    plot.subtitle = element_text(size = 25, hjust = 0),
    plot.caption = element_text(size = 18)
  ) +
  transition_reveal(
    along = year
  ) +
  ease_aes("linear")

a_plot <- animate(plot, fps = 30, duration = 15, end_pause = 30, width = 1000, height = 600)
anim_save(here::here("horror_movies.gif"), a_plot)
