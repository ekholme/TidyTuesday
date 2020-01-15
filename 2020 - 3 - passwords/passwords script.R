library(tidyverse)
library(tidytext)
library(widyr)
library(ggthemes)
library(extrafont)

cyan <- "#2aa198"
violet <- "#6c71c4"

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

pair_strengths <- passwords %>%
  filter(!is.na(rank)) %>%
  unnest_tokens(chars, password, token = "character_shingles", n = 2) %>%
  extract(chars, c("char1", "char2"), "(\\w{1})(\\w{1})") %>%
  mutate(id = row_number()) %>%
  select(id, char1, char2) %>%
  pivot_longer(cols = -id,
               names_to = "names",
               values_to = "letter") %>%
  select(-names) %>%
  pairwise_cor(letter, id, sort = TRUE, upper = FALSE)
  
pair_strengths %>%
  top_n(15) %>%
  mutate(paired_chars = paste0(item1, " : ", item2)) %>%
  ggplot(aes(x = fct_reorder(paired_chars, correlation), y = correlation)) +
    geom_col(fill = cyan) +
    coord_flip() +
    labs(
      x = "",
      y = "Correlation Coefficient",
      title = "If This Character...Then This Character",
      subtitle = "This plot shows how strongly a given first character is associated with a given second character in\ncommon passwords. Because humans are apparently immature creatures, the strongest\nrelationship is between 6 and 9.",
      caption = "Data: Information is Beautiful | Viz: Eric Ekholm (@ekholm_e)"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, .55)
    ) +
    theme_solarized(light = FALSE) +
    theme(
    text = element_text(family = "Rockwell"),
    axis.text = element_text(face = "bold", size = 11, color = violet),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    plot.subtitle = element_text(face = "italic")
    )
