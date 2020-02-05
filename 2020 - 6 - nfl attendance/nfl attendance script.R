set.seed(0408)

library(tidyverse)
library(gghighlight)
library(patchwork)
library(ggtext)

#note -- use the bitter font -- will likely need to install

#also need to set a theme up here

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

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


#ok, so, tentative plan is to use the standings data to make a 'master' plot with the yearly wins for all teams and then
#to break out into small multiples below. Can also include some highlighting of interesting trends in the 'master' plot

#also note that year goes from 2000 - 2019
standings <- standings %>%
  group_by(team_name) %>%
  summarize(total_wins = sum(wins, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(x = standings, y = ., by = "team_name")


main_plot_raw <- standings %>%
  ggplot(aes(x = year, y = wins, color = team_name)) +
  geom_line(size = 2) +
  gghighlight(is.element(total_wins, range(total_wins)), use_direct_label = FALSE, 
              unhighlighted_params = list(size = 1.5, colour = alpha("grey85", 0.6))) +
  theme_minimal() +
  scale_color_manual(
    values = nfl_colors
  )

standings %>%
  ggplot(aes(x = year, y = wins, color = team_name)) +
  geom_line(size = 1) +
  facet_wrap(~ team_name, ncol = 8) +
  theme_minimal() +
  scale_color_manual(
    values = nfl_colors
  ) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_markdown(
      aes(color = team_name)
    )
  )
  