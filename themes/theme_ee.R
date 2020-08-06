
##Personalized Theme for ggplot2
#see https://github.com/Z3tt/TidyTuesday/blob/master/theme/tidy_grey.R for example
library(ggplot2)
library(ggtext)

#approach for fonts may be to just post-hoc this in scripts


theme_ee <- function(size = 11.5, base_font = "sans", base_color = "grey10", accent_color = "grey40") {
  half <- size / 2
  
  theme(line = element_line(color = base_color, size = .3, linetype = 1),
        rect = element_rect(fill = "white", color = base_color),
        text = element_text(family = base_font, face = "plain", color = base_color, size = size, lineheight = .9),
        axis.line = element_blank(),
        axis.text = element_markdown(size = size * .8),
        axis.ticks = element_line(color = base_color),
        axis.ticks.length = unit(half*.25, "pt"),
        axis.title = element_markdown(size = size * 1.1),
        legend.background = element_rect(color = NA),
        legend.margin = margin(.2, .2, .2, .2, "cm"),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(1.25, "lines"),
        legend.text = element_text(size = rel(.9)),
        legend.title = element_text(hjust = 0, size = rel(1)),
        panel.background = element_rect(fill = NA, color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(size = .2, color = accent_color),
        panel.grid.minor = element_line(size = .2, color = accent_color),
        panel.spacing = unit(1.5*size, "pt"),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_markdown(color = base_color, size = size, face = "bold"),
        strip.text.x = element_markdown(margin = margin(t = half, b = half)),
        strip.text.y = element_markdown(margin = margin(l = half, r = half)),
        plot.background = element_rect(color = NA),
        plot.title = element_markdown(size = 2*size, face = "bold", margin = margin(b = .75*size)),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = size, margin = margin(b = .75*size)),
        plot.caption = element_markdown(size = .8*size, face = "italic"),
        plot.caption.position = "plot",
        plot.margin = margin(t = 3*size, b = 3*size, l = 1.5*size, r = 1.5*size)
        )
}


# Theme Dark --------------------------------------------------------------

theme_eedark <- function(size = 11.5, base_font = "sans", base_color = "#cfd8dc", accent_color = "#717786") {
  half <- size / 2
  
  theme(line = element_line(color = base_color, size = .3, linetype = 1),
        rect = element_rect(fill = "#343d52", color = base_color),
        text = element_text(family = base_font, face = "plain", color = base_color, size = size, lineheight = .9),
        axis.line = element_blank(),
        axis.text = element_markdown(size = size * .8, color = base_color),
        axis.ticks = element_line(color = base_color),
        axis.ticks.length = unit(half*.25, "pt"),
        axis.title = element_markdown(size = size * 1.1),
        legend.background = element_rect(color = NA),
        legend.margin = margin(.2, .2, .2, .2, "cm"),
        legend.key = element_rect(fill = "#343d52"),
        legend.key.size = unit(1.25, "lines"),
        legend.text = element_text(size = rel(.9)),
        legend.title = element_text(hjust = 0, size = rel(1)),
        panel.background = element_rect(fill = NA, color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(size = .2, color = accent_color),
        panel.grid.minor = element_line(size = .2, color = accent_color),
        panel.spacing = unit(1.5*size, "pt"),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_markdown(color = base_color, size = size, face = "bold"),
        strip.text.x = element_markdown(margin = margin(t = half, b = half)),
        strip.text.y = element_markdown(margin = margin(l = half, r = half)),
        plot.background = element_rect(color = NA),
        plot.title = element_markdown(size = 2*size, face = "bold", margin = margin(b = .75*size)),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = size, margin = margin(b = .75*size)),
        plot.caption = element_markdown(size = .8*size, face = "italic"),
        plot.caption.position = "plot",
        plot.margin = margin(t = 3*size, b = 3*size, l = 1.5*size, r = 1.5*size)
  )
}

#testing
library(tidyverse)

ggplot(diamonds, aes(x = x, y = y, color = cut)) +
  geom_point() +
  #facet_wrap(~cut) +
  labs(
    title = "test",
    subtitle = "more testing here. Making it a little bit longer.",
    caption = "by bret bretterson",
    x = "Snooze",
    y = "Snoz"
  ) +
  theme_eedark()
