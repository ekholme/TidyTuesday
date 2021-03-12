
# Setup -------------------------------------------------------------------


library(tidyverse)
library(eemisc)
library(ggtext)

#read in data
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')



# Plot --------------------------------------------------------------------



df <- income_limits %>%
  filter(dollar_type == "2019 Dollars",
         race == "All Races") %>%
  pivot_wider(names_from = income_quintile,
              values_from = income_dollars) %>%
  mutate(disparity = `Top 5%`/Lowest)

#getting first and last points to add to text
disp_first <- round(df$disparity[df$year == 1967], 2)
disp_last <- round(df$disparity[df$year == 2019], 2)

p <- df %>%
  ggplot(aes(x = year, y = disparity)) +
  geom_line(size = 2) +
  annotate("richtext", x = 1967, y = 5,
           label = "In 1967, an American family<br>in the top 5% of income<br>earned **<span style='color:#660012'>6.3x</span>** more than<br>a family in the bottom 20%.", hjust = 0, label.color = NA, fill = NA) +
  annotate("richtext", x = 2019, y = 8.3,
           label = "In 2019, this<br>disparity was **<span style='color:#660012'>9.6x</span>**",
           hjust = 1, label.color = NA, fill = NA) +
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
    title = "Income Inequality Continues to Grow",
    caption = "Data: Urban Institute & US Census | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme_ee() +
  theme(
    panel.grid.minor = element_blank(),
    #panel.grid.major = element_line(color = "grey80"),
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    plot.title = element_markdown(size = 25)
  )
  
 ggsave(here::here("2021 - 7 - wealth and income/income_inequality.png"), p, device = "png") 
 