library(tidytuesdayR)
library(tidyverse)
library(leaflet)

# Read in Tidy Tuesday data
tt <- tt_load(2020, week = 9)
raw_measles <- tt$measles

# Clean the data

states <- us_map() %>% select(full, abbr) %>% unique()

measles_bar <- raw_measles %>%
filter(overall > 0) %>%
mutate_at("state", as.factor) %>%
group_by(state) %>%
summarise("Avg_Vac_Perc" = mean(overall)) %>%
arrange(desc(Avg_Vac_Perc)) %>%
mutate(fips = fips(state)) %>%
left_join(y = states, by = c("state"="full"))


measles_bar %>%
  ggplot() + 
  geom_bar(aes(x = reorder(state,Avg_Vac_Perc), y = Avg_Vac_Perc), stat = "identity") + coord_flip(ylim = c(50,100)) + xlab("State") + ylab("Average Overall Vaccination Rate")



library(usmap)
library(wesanderson)

plot_usmap(
  data = measles_bar,
  values = "Avg_Vac_Perc",
  alpha = 0.9,
  color = "black",
  labels = TRUE
) +
  scale_fill_continuous(name = "Overall Vaccination Rate", low = "red", high = "green") +
  theme(legend.position = "right") +
  ggtitle(label = "Top Vaccination Rates Across the United States") +
  theme(panel.background = element_rect(color = "black", fill = "khaki"))
           