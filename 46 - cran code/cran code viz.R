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
  filter(rank <= 10 & is.finite(comment_code_ratio) & code > median(.$code) & comment_code_ratio <= 1) %>%
  mutate(language = fct_reorder(language, comment_code_ratio, mean, .desc = TRUE))

ggplot(cran, aes(x = comment_code_ratio, y = language)) +
  geom_density_ridges(color = "white", fill = "white", alpha = .8) +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Comment:Code Ratio",
    y = "Language",
    title = "Comment to Code Ratio of R Packages, by Language"
  ) +
  theme(
    rect = element_rect(fill = "black"),
    text = element_text(family = "mono", color = "white"),
    panel.background = element_rect(fill = NA),
    axis.text = element_text(color = "white", family = "mono"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )


#notes to self:
##plan is to make this simple but to have the graphic look like a command line prompt
##or maybe like the RStudio twilight interface?