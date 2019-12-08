library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(broom)
library(hrbrthemes)

set.seed(0408)

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

#cleaning steps
tickets <- tickets %>%
  mutate(issue_date = date(issue_datetime),
         month = month(issue_date),
         day = day(issue_date),
         day_of_week = wday(issue_date, label = TRUE),
         weekend = if_else(str_detect(day_of_week, "Sun|Sat"), "Y", "N"),
         violation_desc = tolower(violation_desc)) %>%
  mutate(violation_desc = str_replace_all(violation_desc, " cc", ""))

keep_vios <- tickets %>%
  count(violation_desc, sort = TRUE) %>%
  top_n(4) %>%
  select(violation_desc) %>%
  as_vector()

tix_viz <- tickets %>%
  filter(is.element(violation_desc, keep_vios)) %>%
  count(issue_date, violation_desc) %>%
  mutate(n = replace_na(n, 0)) %>%
  group_by(violation_desc) %>%
  nest() %>%
  mutate(ts_obj = map(data, ~zoo(.$n, order.by = .$issue_date) %>% as.ts()),
         acf_mod = map(ts_obj, ~acf(., lag.max = 7, na.action = na.pass)),
         acf_tidy = map(acf_mod, ~tidy(.))) %>%
  unnest(acf_tidy) %>%
  filter(lag != 0) %>%
  ungroup() %>%
  mutate(violation_desc = tools::toTitleCase(violation_desc) %>%
           str_replace_all("over", "Over")) %>%
  ggplot(aes(x = fct_relevel(as.factor(lag), as.character(c(7:1))), y = acf, fill = violation_desc,
             color = violation_desc)) +
  geom_col() +
  facet_wrap(vars(violation_desc)) +
  coord_flip() +
  theme_ft_rc() +
  scale_color_ft() +
  scale_fill_ft() +
  labs(
    title = "Autocorrelations among Number of Parking Tickets Issued Daily",
    subtitle = "We can see different patterns of autocorrelation among the most common ticket types issued in\nPhilly. Across all types, however, one day and one week lags have the strongest correlations",
    y = "Correlation Coefficient",
    x = "# of Days Lagged",
    caption = "Data: Open Data Philly | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(hjust = .5, size = 12),
    axis.title.y = element_text(hjust = .5, size = 12, margin = margin(r = 10))
  )

ggsave(here::here("49 - philly parking/philly parking viz.jpeg"), plot = tix_viz, device = "jpeg")


