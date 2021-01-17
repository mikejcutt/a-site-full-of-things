library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(scales)


eruptions <-
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

volcano <-
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

Volcano_pops <-
  left_join(volcano,
    mutate_at(eruptions, "start_year", as.character),
    by = c("volcano_name", "last_eruption_year" = "start_year")
  ) %>%
  filter(!is.na(vei)) %>% 
  filter(across(contains("population")) > 0) %>% 
  mutate_at("vei", as.factor)


CPCOLS <-
  c("#261703FF",
    "#2E1D05FF",
    "#402808FF",
    "#E0B300FF",
    "#FA6400FF",
    "#E61300FF",
    "#801107",
    "#4A053FFF",
    "#4A053FFF")


volc_pops_plot <-  ggplot(Volcano_pops) +
  geom_histogram(color = NA, aes(x = population_within_5_km, fill = vei)) +
  scale_x_log10(labels = comma) +
  scale_fill_manual(
    values = CPCOLS,
    name = "Volcanic Explosivity Index",
    labels = c(
      "0 (Effusive)",
      "1 (Gentle)",
      "2 (Explosive)",
      "3 (Catastrophic)",
      "4 (Cataclysmic)",
      "5 (Paroxysmic)",
      "6 (Colossal)",
      "7 (Super-colossal)"
    )
  ) +
  theme(
    plot.background = element_rect(fill = "#222222", color = "#222222"),
    panel.background = element_rect(fill = "#222222", color = "#222222"),
    axis.ticks.x = element_line(
      colour = "white",
      size = 1,
      lineend = "square"
    ),
    axis.ticks.y = element_line(colour = "white",
                                size = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = "Century Gothic",
      size = 18,
      color = "white"
    ),
    plot.subtitle = element_text(
      family = "Century Gothic",
      size = 9.5,
      color = "#D9D9D9",
      face = "italic"
    ),
    legend.title = element_text(
      family = "Century Gothic",
      size = 13,
      color = "white"
    ),
    legend.background = element_rect(fill = "#222222", color = "#222222"),
    legend.key = element_blank(),
    legend.text = element_text(
      family = "Century Gothic",
      size = 11,
      color = "white"
    ),
    axis.text.x = element_text(
      family = "Century Gothic",
      size = 11,
      color = "white"
    ),
    axis.text.y = element_text(
      family = "Century Gothic",
      size = 11,
      color = "white"
    ),
    axis.title.x = element_text(
      family = "Century Gothic",
      size = 12,
      color = "white",
      face = "bold"
    ),
    axis.title.y = element_text(
      family = "Century Gothic",
      size = 12,
      color = "white",
      face = "bold"
    )
  ) +
  labs(
    x = "\nPopulation within 5km of Eruption Site",
    y = "Frequency\n",
    title = "Living in the Danger Zone",
    subtitle = "The most recent eruptions of each volcano around the globe,\nand the number of people that were living dangerously close to the eruption site"
  )

volc_pops_plot

