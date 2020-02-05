set.seed(0408)

library(tidyverse)
library(gghighlight)
library(colorr) #for nfl color palette

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

teams <- c("cardinals",  "falcons",  "ravens",  "bills",  "panthers",  "bears",  "bengals",  "browns",
           "cowboys", "broncos", "lions", "packers", "texans", "colts", "jaguars", "chiefs", "chargers", "rams",
           "dolphins", "vikings", "patriots", "saints", "giants", "jets", "raiders", "eagles", "steelers", 
           "niners","seahawks", "buccaneers", "titans", "redskins")

color_tbl <- tibble(
  team = teams,
  colz = map(teams, ~nfl.colors(.))
) %>%
  unnest(cols = colz) %>%
  filter(!is.element(colz, c("#000000", "#ffffff")))
#next -- filter out black '#000000' and white and then keep distinct


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