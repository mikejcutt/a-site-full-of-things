library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(scales)


spotify_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify <- spotify_raw %>%
  mutate(track_album_release_year = str_sub(track_album_release_date, start = 1, end = 4) %>% as.numeric) %>%
  filter(track_album_release_year >= 1970) %>%
  mutate(
    era = case_when(
      track_album_release_year %>% between(1970, 1979) ~ "'70s",
      track_album_release_year %>% between(1980, 1989) ~ "'80s",
      track_album_release_year %>% between(1990, 1999) ~ "'90s",
      track_album_release_year %>% between(2000, 2009) ~ "'00s",
      track_album_release_year %>% between(2010, 2019) ~ "'10s",
      track_album_release_year %>% between(2020, 2029) ~ "'20s",
    ) %>%
      fct_relevel(., "'70s", "'80s", "'90s", "'00s", "'10s", "'20s"),
    playlist_genre = as.factor(playlist_genre)
  ) %>%
  arrange(track_album_release_year)

levels(spotify$playlist_genre) <- c("EDM","Latin","Pop","R&B","Rap","Rock")


CPCOLS <- c("#1E4A35FF","#295E44","#25664A","#266E49FF","#2B7850","#399E6A","#3D7840")


genres_over_time <- 
  spotify %>%
  group_by(era) %>%
  ggplot() +
  geom_boxplot(aes(x = era, y = track_popularity, fill = era), colour = "#AFED8B", size = 0.6,
               outlier.colour = "#AFED8B", outlier.shape = 16, outlier.size = 1.25) +
  facet_wrap( ~ playlist_genre) +
  scale_fill_manual(name = "Era", values = CPCOLS[1:6]) +
  theme(
    plot.background = element_rect(fill = "#222222", color = "#222222"),
    panel.background = element_rect(fill = "#222222", color = "#222222"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#15883e", size = 0.01),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(colour = "#15883e",size = 1,lineend = "square"),
    axis.text = element_text(family = "Century Gothic",colour = "white", size = 9),
    strip.background = element_rect(colour = "#15883e", fill = CPCOLS[7]),
    strip.text = element_text(family = "Century Gothic",colour = "white", size = 12),
    axis.title.x = element_text(family = "Century Gothic",colour = "white", size = 14),
    axis.title.y = element_text(family = "Century Gothic",colour = "white", size = 14),
    plot.title = element_text(family = "Century Gothic",colour = "white", size = 18),
    plot.caption = element_text(family = "Century Gothic",colour = "white", size = 8),
    panel.border = element_rect(colour = "#15883e", fill = NA),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 50, b = 5, l = 5, unit = "pt"),
    panel.spacing = unit(0.75, "lines")
  ) +
  labs(x = "\nMusic Era", y = "Track Popularity\n", title = "Song Popularity by Genre and Era",
       subtitle = "",caption = "Source: spotifyr - Song Genres\nVisualisation by Michael Cutter")

genres_over_time

