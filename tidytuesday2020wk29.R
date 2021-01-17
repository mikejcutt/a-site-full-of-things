library(tidytuesdayR)
library(tidyverse)
library(gganimate)
library(transformr)
library(extrafont)
library(scales)
library(rsvg)

# import fonts - only once
#font_import()
# load fonts - every session
loadfonts(device = "win", quiet = TRUE)


astronauts_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts <- astronauts_raw %>%
  mutate(nation_final = case_when(nationality == "U.S." ~ "U.S.", 
                                  nationality == "U.S.S.R/Russia" ~ "U.S.S.R/Russia",
                                  TRUE ~ "All other nations"))
space_race <- astronauts %>% 
  select(id, nation_final, year_of_mission)

space_race <- space_race %>%
  group_by(nation_final, year_of_mission) %>%
  summarise(astronaut_count = n()) %>%
  mutate(cum_astronaut_count = cumsum(astronaut_count)) %>%
  arrange(year_of_mission) %>%
  ungroup() %>%
  mutate(year_id = group_indices(., year_of_mission),
         show_time = ifelse(year_of_mission == 1978, 20, 1)) %>%
  uncount(show_time)

space_race <- space_race %>%
  mutate(pause_year = ifelse(year_of_mission == 1978, "pause","no_pause")) %>%
  group_by(nation_final, pause_year) %>%
  mutate(reveal_time = ifelse(pause_year == "pause", row_number(), year_id)) %>%
  ungroup()

space_race <- space_race %>%
  group_by(nation_final, pause_year) %>%
  mutate(reveal_time_final = case_when(
    # in the year I want to pause at, I need to add 17 (the frame number before the pause) 
    # to the row number generated for frames within the pause
    pause_year == "pause" ~ reveal_time + 17L,
    # the pause is for 20 frames, so I need to then add 19 to the frames to the rownumber after the pause
    pause_year == "no_pause" & year_of_mission > 1978 ~ year_id + 19L,
    TRUE ~ reveal_time)
         ) %>%
  ungroup() %>%
  mutate(reveal_time_final_label = paste(year_of_mission, reveal_time_final, sep = "_"),
         text = ifelse(year_of_mission == 1978, 
                       "Space representation begins to widen in 1978,\n with Astronauts from Czechoslovakia, Germany,\n and Poland joining missions in space"," "))

CPCOLS <- c("#71FF5EFF","#5C5CE6FF","#D90707FF")

space_race_plot <- space_race %>%
  ggplot(aes(x = year_of_mission, y = cum_astronaut_count, 
             colour = factor(nation_final), label = text)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  geom_text(aes(x = 1960, y = 200), size = 4, hjust = 0, colour = "white") +
  scale_colour_manual(
    values = CPCOLS,
    name = "Nation") +
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
    plot.caption = element_text(
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
  scale_x_continuous(limits = c(NA, 2035)) +
  scale_y_continuous(limits = c(-100, NA)) +
  labs(y = "No. of Astronauts", x = "", title = "The Space Representation Race", 
       subtitle = "The number of astronauts on space missions by nation", 
       caption = "Graphic: Michael Cutter Data: Astronaut Database")

space_race_plot_anim <- space_race_plot + 
  transition_reveal(reveal_time_final)

anim_save(filename = "space_race_plot_anim.gif", space_race_plot_anim, duration = 20, start_pause = 3, end_pause = 6)



