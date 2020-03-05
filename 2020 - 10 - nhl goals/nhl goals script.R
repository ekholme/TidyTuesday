
library(tidyverse)
library(ggtext)
library(extrafont)

###possibilities
# Who has Ovechkin scored against the most? -- let's do this. Assists/goals by opponent

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

caps_blue <- "#041e42"
caps_red <- "#c8102e"

ovi <- game_goals %>%
  filter(player == "Alex Ovechkin") %>%
  mutate(opp = if_else(opp == "MDA", "ANA", opp)) %>%
  group_by(opp) %>%
  summarize(goals = sum(goals, na.rm = TRUE),
            assists = -1*sum(assists, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  mutate_at(vars(c("goals", "assists")), list(per_game = ~round(./n, digits = 2))) %>%
  mutate(points_per_game = goals_per_game - assists_per_game)

ovi %>%
  ggplot() +
    geom_col(aes(x = fct_reorder(opp, points_per_game),
                 y = goals_per_game), fill = caps_red) +
    geom_col(aes(x = fct_reorder(opp, points_per_game),
               y = assists_per_game), fill = caps_blue) +
    geom_text(aes(x = fct_reorder(opp, points_per_game),
                  y = goals_per_game-.01, label = goals_per_game), hjust = 1, color = "white", fontface = "bold") +
  geom_text(aes(x = fct_reorder(opp, points_per_game),
                y = assists_per_game+.01, label = if_else(-1*assists_per_game == 0, "", paste0(-1*assists_per_game))), 
            hjust = 0, color = "white", fontface = "bold") +
  geom_text(aes(x = fct_reorder(opp, points_per_game),
                y = 0, label = opp), 
            hjust = 0, color = "white", fontface = "bold") +
  coord_flip() +
  #annotate("text", x = nrow(ovi)+2, y = max(ovi$goals_per_game) , label = "Goals", size = 15, color = caps_red, 
  #         fontface = "italic", hjust = 1) +
  geom_richtext(x = nrow(ovi)+1.5, y = max(ovi$goals_per_game), label = "<span style='font-size:40pt'>Goals</span>
               <span style='font-size:10pt'>per game</span>", color = caps_red,
               fontface = "italic", hjust = 1, label.color = NA, fill = NA,
               label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(x = nrow(ovi)+1.5, y = min(ovi$assists_per_game), label = "<span style='font-size:40pt'>Assists</span>
               <span style='font-size:10pt'>per game</span>", color = caps_blue,
                fontface = "italic", hjust = 0, label.color = NA, fill = NA,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  #annotate("text", x = nrow(ovi)+2, y = min(ovi$assists_per_game) , label = "Assists", size = 15, color = caps_blue, 
  #         fontface = "italic", hjust = 0) +
  theme_void() +
  scale_x_discrete(
    expand = expand_scale(add = c(2, 6))
  ) +
  labs(
    title = "Who does Ovi play best against?",
    subtitle = "This plot shows Alex Ovechkin's <span style='color:#041e42'>assists per game</span> and <span style='color:#c8102e'>goals per game</span> against opponents throughout his career. His best games seem to be against Minnesota, where he averages 1.63 points (goals + assists) per game.",
    caption = "Data: HockeyReference.com & The Washington Post | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme(
    text = element_text(family = "Lucida Sans", lineheight = 0),
    plot.title = element_markdown(size = 25, face = "bold"),
    plot.subtitle = element_textbox(size = 12, face = "italic"),
    panel.background = element_rect(fill = "grey85"),
    plot.background = element_rect(fill = "grey85")
  )
  
