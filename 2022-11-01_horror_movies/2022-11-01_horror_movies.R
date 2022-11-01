# Load libraries

library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(scales)

# Load Data

data <- tt_load("2022-11-01")
films <- data %>% pluck(1)

# Manipulate

rev_films <- films %>% 
  mutate(
    revenue_pct = revenue / budget,
    decade = year(release_date) - year(release_date) %% 10,
    decade = fct_reorder(as_factor(decade), release_date)
  ) %>% 

rev_vote_films <- rev_films %>% 
  filter(vote_average != 0 & vote_count != 0)

# Visualize

rev_films %>% 
  ggplot(aes(vote_count, revenue_pct)) + 
  geom_point() +
  geom_smooth(se = TRUE) + 
  geom_hline(yintercept = 1) +
  scale_y_log10(labels = c(0.01, 0.1, 1, 10, 100, 1000), breaks = c(0.01, 0.1, 1, 10, 100, 1000)) +
  scale_x_log10(labels = label_number()) + 
  facet_wrap(~ decade) 

# Save

ggsave(path = "2022-11-01_horror_movies", filename = "2022-11-01_horror_movie.png", scale = 1.778)
ggsave(path = "plots", filename = "2022-11-01_horror_movie.png", scale = 1.778)