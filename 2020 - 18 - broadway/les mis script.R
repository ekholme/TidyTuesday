
library(tidyverse)
library(ggtext)
library(hrbrthemes)
library(ggridges)

theme_set(theme_ipsum())

red <- '#cd0619'
blue <- '#031964'
bckgrnd <- '#373737'
text_col <- '#fdfdfd'

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

les_mis <- grosses %>%
  filter(show == "Les Miserables") %>%
  mutate(tix_cumul = cumsum(seats_sold))

all_tix <- max(les_mis$tix_cumul)

les_mis %>%
  ggplot(aes(x = week_ending, y = 1, height = tix_cumul/1000, fill = (tix_cumul/1000)^1.05)) +
  geom_ridgeline_gradient() +
  scale_fill_gradient(
    low = red,
    high = blue
  ) +
  labs(
    y = "Cumulative Tickets Sold (in thousands)",
    x = "Year",
    title = "Do You Hear the People Sing?",
    subtitle = "Broadway ticket sales for Les Miserables were consistent until the show went off<br>Broadway in the early 2000s. The release of the Les Mis movie sparked a return of the<br>show in 2014. The show has sold over 10.5 million tickets on Broadway since it debuted.",
    caption = "Data: Playbill | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill = bckgrnd),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(color = text_col, family = "Bahnschrift"),
    axis.text = element_text(color = text_col, family = "Bahnschrift"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "Bahnschrift", size = 11),
    plot.title = element_markdown(family = "Bahnschrift")
  )

ggsave(here::here("2020 - 18 - broadway/les_mis.png"), device = "png")
