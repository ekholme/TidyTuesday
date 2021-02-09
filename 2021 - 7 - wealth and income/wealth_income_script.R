
# Setup -------------------------------------------------------------------


library(tidyverse)
library(eemisc)


#read in data
lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')



# Retirement Plot ---------------------------------------------------------

retirement %>%
  ggplot(aes(x = year, y = retirement, color = race)) +
  geom_line() +
  theme_ee()


# Home Owner --------------------------------------------------------------

home_owner %>%
  ggplot(aes(x = year, y = home_owner_pct, color = race)) +
  geom_line() +
  theme_ee()



# Income Time -------------------------------------------------------------


income_limits %>%
  mutate(race = case_when(
    str_detect(race, "White") ~ "White",
    str_detect(race, "Black") ~ "Black",
    TRUE ~ race
  )) %>%
  filter(dollar_type == "2019 Dollars",
         race %in% c("Black", "Hispanic", "White"))


####new idea : plot the ratio of 90% all race income vs 10% all race income over time to show inequality
#could also do each race as a greyed-out subplot in the same plot

income_limits %>%
  filter(dollar_type == "2019 Dollars",
         race == "All Races") %>%
  pivot_wider(names_from = income_quintile,
              values_from = income_dollars) %>%
  mutate(disparity = `Top 5%`/Lowest) %>%
  ggplot(aes(x = year, y = disparity)) +
  geom_line(size = 2) +
  scale_y_continuous(
    limits = c(0, 10),
    labels = function(x) paste0(x, "x")
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, by = 10)
  ) +
  labs(
    y = "Income Disparity",
    x = NULL,
    title = "Income Inequality Continues to Grow"
  ) +
  theme_ee() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  )
  
  