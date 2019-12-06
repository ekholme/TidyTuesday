library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)

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

tix_ts <- tickets %>%
  filter(is.element(violation_desc, keep_vios)) %>%
  count(issue_date, violation_desc) %>%
  mutate(n = replace_na(n, 0)) %>%
  group_by(violation_desc) %>%
  nest() %>%
  mutate(ts_obj = map(data, ~zoo(.$n, order.by = .$issue_date) %>% as.ts()),
         auto_mod = map(ts_obj, ~auto.arima(.)))
#leaving off here for now -- need to figure out how to broom this next




