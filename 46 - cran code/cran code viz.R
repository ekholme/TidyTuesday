#visualizing data from CRAN code

library(tidyverse)
library(ggridges)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

cran <- cran_code %>%
  select(pkg_name, everything()) %>%
  mutate(comment_code_ratio = comment/code) %>% #let's keep this simple and look at distributions of comments to lines of code
  group_by(language) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(count))) %>% #this will give a ranking where 1 = the most common in the dataset, etc.
  filter(rank <= 10) %>%
  filter(is.finite(comment_code_ratio))
#will need to add in another filter to get rid of outliers

ggplot(cran, aes(x = comment_code_ratio, y = language)) +
  geom_density_ridges()