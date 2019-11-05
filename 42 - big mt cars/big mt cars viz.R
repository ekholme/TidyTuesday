
library(tidyverse)
library(broom)
library(janitor)
library(ggimage)

cars_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

#plan is to create a model for each make where year predicts fuel efficiency, then plot the slopes of these models
#using a lollipop plot. Want to get some practice with nesting using tidyr and using the broom package

#variables I want are co2titlepipe_gpm, year, make. I think I also want to filter by fueltype1 = regulat gasoline

cars <- cars_raw %>%
  clean_names(case = "snake") %>%
  select(year, make, model, co2tailpipe_gpm, fuel_type1) %>%
  filter(fuel_type1 == "Regular Gasoline") %>%
  #next, I want to get counts of each make
  group_by(make) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(count))) %>% #this will give a ranking where 1 = the most common in the dataset, etc.
  filter(rank <= 10) %>%
  mutate(year_84 = year - 1984)

#let's take a look at counts by make now
cars %>%
  distinct(make, count, rank) %>%
  arrange(desc(count))

#let's also take a quick look at the year range in this dataset
range(cars$year) #1984 thru 2020, so about 26 years.

#ok, let's nest within make
cars <- cars %>%
  group_by(make) %>%
  nest()

make_mod <- function(df) {
  lm(co2tailpipe_gpm ~ year_84, data = df)
}

model_df <- cars %>%
  mutate(
    mod = map(data, make_mod)
  ) %>%
  mutate(
    rsq = map(mod, glance) %>%
      map_dbl("r.squared"), #using map_dbl here with the name of the vector functions to extract this column
    betas = map(mod, tidy)
  ) %>%
  unnest(betas) %>%
  select(-c("std.error", "statistic", "p.value")) %>%
  mutate(term = if_else(
    str_detect(term, "Int"), "intercept", term
  )) %>%
  spread(key = term, value = estimate)

#creating lollipop chart
ggplot(model_df, aes(x = reorder(make, -year_84),
                     y = -1*year_84)) +
  geom_point(aes(size = intercept), color = "white") +
  geom_segment(aes(x = make, xend = make, y = 0, yend = -1*year_84), size = 1, linetype = "longdash", color = "white") +
  theme_classic() +
  scale_y_continuous(
    limits = c(0, 5),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Estimated Annual Decrease in Tailpipe CO2 Emissions (grams/mile), 1984-2020",
    title = "Who's Getting Cleaner?",
    subtitle = paste0("Ford, Jeep, and Dodge have the largest annual reduction in CO2 emissions since 1984.\nHowever, Honda had much lower average emissions in 1984 and therefore had less room to improve.")
  ) +
  scale_size_continuous(
    name = "CO2 Emissions in 1984"
  ) +
  theme(
    panel.background = element_rect(fill = "grey20"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey20"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", size = 10),
    axis.line = element_line(color = "gold", size = 1.5),
    legend.background = element_rect(fill = "grey20"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0)
  ) +
  coord_flip()
  