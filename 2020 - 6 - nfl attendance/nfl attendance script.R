set.seed(0408)

library(tidyverse)
library(janitor)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

#ok, so, tentative plan is to use the standings data to make a 'master' plot with the yearly wins for all teams and then
#to break out into small multiples below. Can also include some highlighting of interesting trends in the 'master' plot