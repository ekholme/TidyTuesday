set.seed(0408)

library(tidyverse)
library(patchwork)
library(ggtext)
library(sysfonts)
library(extrafont)
library(showtext)

font_add_google("Rubik", "rubik")

showtext_auto()

theme_set(theme_minimal())

theme_update(
  legend.position = "none",
  text = element_text(family = "rubik")
)

standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

teams <- c("cardinals",  "falcons",  "ravens",  "bills",  "panthers",  "bears",  "bengals",  "browns",
           "cowboys", "broncos", "lions", "packers", "texans", "colts", "jaguars", "chiefs", "chargers", "rams",
           "dolphins", "vikings", "patriots", "saints", "giants", "jets", "raiders", "eagles", "steelers", 
           "49ers","seahawks", "buccaneers", "titans", "redskins") %>%
  str_to_title()

nfl_colors <-  c("#97233f", "#a71930", "#241773", "#00338d", "#0085ca", "#0b162a",
           "#fb4f14", "#ff3c00", "#041e42", "#002244", "#0076b6", "#203731", 
           "#03202f", "#002c5f", "#006778", "#e31837", "#002a5e", "#002244", 
           "#008e97", "#4f2683", "#002244", "#d3bc8d", "#0b2265", "#125740", 
           "#000000", "#004c54", "#ffb612", "#aa0000", "#69be28", "#d50a0a", 
           "#4b92db", "#773141")

names(nfl_colors) <- teams

colors_tbl <- tibble(
  team_name = teams,
  color = nfl_colors
)

standings <- standings %>%
  group_by(team_name) %>%
  summarize(total_wins = sum(wins, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(x = standings, y = ., by = "team_name")

main_bar <- standings %>%
  distinct(team_name, total_wins) %>%
  ggplot(aes(x = fct_reorder(team_name, total_wins), y = total_wins, color = team_name, fill = team_name)) +
  geom_col() +
  scale_fill_manual(
    values = nfl_colors
  ) +
  scale_color_manual(
    values = nfl_colors
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Wins",
    title = "Total Wins"
  ) +
  coord_flip() +
  theme(
    plot.title = element_text(family = "rubik"),
    panel.grid = element_blank()
  )

facet_plot <- standings %>%
  ggplot(aes(x = year, y = wins, color = team_name)) +
  geom_line(size = 1) +
  facet_wrap(~ team_name, ncol = 8) +
  theme_minimal() +
  scale_color_manual(
    values = nfl_colors
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  labs(
    title = "Wins by Season"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(family = "rubik"),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

full_plot <- main_bar + facet_plot + 
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
    title = "NFL Team Performance, 2000 - 2019",
    subtitle = "These plots show the performance of all 32 NFL teams over the past two decades. In the left panel, we can see total wins from 2000 - 2019. The <span style='color:#002244'>Patriots</span> have won 237 games, whereas the <span style='color:#ff3c00'>Browns</span> have only won 99. In the right panel, we can see the performance of each team over time.",
    caption = "Data: Pro Football Reference | Viz: Eric Ekholm (@ekholm_e)"
  ) &
  theme(
    text = element_text(family = "rubik"),
    plot.title = element_text(size = 20, hjust = .5),
    plot.subtitle = element_textbox()
  )

x11()
print(full_plot)
