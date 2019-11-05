#TidyTuesday week 45 viz
#modes less traveled using ACS Data

library(tidyverse)
library(patchwork)

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

#I think my plan for this one is to do something relatively simple and really try to make it look good.
#plus to play around with patchwork

commute <- commute_mode %>%
  tidyr::extract(col = city, 
          into = c("city_name", "city_descrip"),
          regex = "(.*) (.*$)")
