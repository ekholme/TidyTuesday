#Viz file for powerlifting tidytuesday data

library(tidyverse)
library(ggbeeswarm)
library(scales)
library(extrafont)
#font_import()
loadfonts(device = "win")

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

#adding in missplot function to see missing variables
miss_plot <- function(data, color1 = "steelblue1", color2 = "steelblue4", bound = 0) {
  miss_tab <<- tibble(
    column = names(data),
    perc_miss = map_dbl(data, function(x) sum(is.na(x))/length(x))
  ) %>%
    filter(perc_miss > bound)
  
  ggplot(miss_tab, aes(x = column, y = perc_miss)) +
    geom_bar(stat = "identity", aes(fill = ..y..)) +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    scale_fill_gradient(low = color1, high = color2, name = "Percent Missing") +
    labs(
      title = "Missingness by Variable",
      y = "Percent Missing",
      x = "Variables"
    )
}

miss_plot(df) #to explore missingness

#let's try a grouped violin male/female by all three lifts
#will also keep date and name for now just in case it's interesting
lifts <- df %>%
  select(name, sex, matches("best3"), date) %>%
  rename_at(vars(matches("best")), list(~str_replace_all(., "best3(\\w+)_kg", "\\1"))) %>%
  drop_na() %>% #will drop any rows with missing values
  gather(key = "lift", value = "weight", -c("name", "sex", "date")) %>%
  filter_if(is.numeric, all_vars(. > 0)) %>% #cleaning up some weird negative numbers
  mutate(lift = tools::toTitleCase(lift)) #putting in title case so axis looks cleaner


#creating viz
ggplot(lifts,
       aes(x = fct_relevel(lift, "Bench", "Squat", "Deadlift"),
           y = weight, 
           color = sex, 
           group = sex)) +
  geom_quasirandom(alpha = .3, width = .1, dodge.width = .5) +
  #stat_summary(fun.y = "median", geom = "errorbar", aes(ymin = ..y.., ymax = ..y..),
   #            size = 1, color = "snow4", position = position_dodge(width = .5),
    #           width = .3) +
  scale_y_continuous(
    limits = c(0, 500),
    expand = c(0, 0),
    labels = scales::unit_format(unit = "kg")
  ) +
  theme_minimal() +
  labs(
    x = "Lift",
    y = "Weight",
    title = "Distribution of Lifts, by Gender"
  ) +
  scale_color_manual(
    values = c("#fb8072", "#80b1d3"),
    labels = c("Female", "Male")
  ) +
  theme(
    legend.title = element_blank(),
    axis.line = element_line(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = unit(c(1, 2, 1, 2), "cm"),
    text = element_text(family = "Calibri"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = .5)
  )
#note that commented out portion will create horizontal bars at median. decided it didn't look great, though
