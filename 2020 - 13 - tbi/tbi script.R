
library(tidyverse)
library(ggtext)
library(extrafont)

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

tbi <- tbi_age %>%
  filter(str_detect(age_group, "17|Total", negate = TRUE)) %>%
  mutate(injury_mechanism = case_when(
    str_detect(injury_mechanism, "Other") ~ "Other",
    str_detect(injury_mechanism, "object") ~ "Struck by Object",
    TRUE ~ injury_mechanism
  ) %>%
    str_to_title()) %>%
  group_by(age_group, injury_mechanism) %>%
  summarize(num = sum(number_est, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(age_group) %>%
  mutate(total = sum(num, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(perc = 100*(num/total),
         age_group = fct_relevel(age_group, "0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))


tbi %>%
  ggplot(aes(x = age_group, y = fct_reorder(injury_mechanism, perc), fill = perc)) +
  geom_tile() +
  scale_fill_viridis_c(option = "B",
                       name = "Percent of TBI, by Age") +
  labs(
    x = "Age Range",
    y = "",
    title = "Walk Carefully!",
    subtitle = "Most traumatic brain injuries (TBI) are caused by unintentional falls, particularly for young children and the elderly.",
    caption = "Data: CDC | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(.75, .5, .75, .5, "cm"),
    plot.background = element_rect(fill = "grey15"),
    axis.text = element_text(color = "white", size = 12),
    axis.title.x = element_text(size = 14),
    panel.grid = element_blank(),
    text = element_text(family = "Rockwell", color = "white"),
    plot.title = element_markdown(size = 26),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 14, face = "italic"),
    legend.position = "bottom"
  )

ggsave(here::here("2020 - 13 - tbi/tbi_heatmap.jpg"), device = "jpeg", width = 11.5, height = 8.5)
