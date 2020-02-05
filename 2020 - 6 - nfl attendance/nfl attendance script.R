set.seed(0408)

library(tidyverse)
library(gghighlight)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

#ok, so, tentative plan is to use the standings data to make a 'master' plot with the yearly wins for all teams and then
#to break out into small multiples below. Can also include some highlighting of interesting trends in the 'master' plot
standings <- standings %>%
  group_by(team_name) %>%
  summarize(total_wins = sum(wins, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(x = standings, y = ., by = "team_name")


standings %>%
  ggplot(aes(x = year, y = wins, color = team_name)) +
  geom_line(size = 2) +
  gghighlight(is.element(total_wins, range(total_wins)), use_direct_label = FALSE, 
              unhighlighted_params = list(size = 1.5, colour = alpha("grey85", 0.8))) +
  theme_minimal()
#ok, so this gets us to a janky version of the main plot -- still need to play around with colors, though