library(tidyverse)
library(eemisc)
library(harrypotter)
library(ggtext)

babynames <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv")

theme_set(theme_ee(accent_color = "gray80"))

kiddos <- babynames %>%
    group_by(year, sex) %>%
    mutate(rank = rank(desc(prop))) %>%
    ungroup() %>%
    filter((name == "Emma" & sex == "F") | (name == "James" & sex == "M")) %>%
    mutate(
        clr = if_else(name == "James", "#E59950", "#660012"),
        nms = glue::glue("<i style='color:{clr}'>{name}</i>")
    )

pts <- kiddos %>%
    group_by(nms) %>%
    filter(year == min(year) | year == max(year) | rank == max(rank)) %>%
    distinct(nms, rank, .keep_all = TRUE)

p <- kiddos %>%
    ggplot(aes(x = year, y = rank, color = nms)) +
    geom_line(size = 1.5) +
    geom_point(
        data = pts, aes(x = year, y = rank, color = nms),
        shape = 21, fill = "white", stroke = 2, size = 7
    ) +
    geom_text(data = pts, aes(x = year, y = rank, label = rank), size = 3) +
    labs(
        y = "Popularity Ranking",
        x = "Year",
        caption = "Data: {babynames} r package | Viz: @ekholm_e"
    ) +
    facet_wrap(vars(nms)) +
    scale_color_hp_d(option = "HermioneGranger") +
    scale_y_reverse() +
    theme(
        strip.text = element_markdown(size = 60),
        legend.position = "none"
    )

ggsave(here::here("2022 - 12 - baby_names/kiddos.png"), p, device = "png")
