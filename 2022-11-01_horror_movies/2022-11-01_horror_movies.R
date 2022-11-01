# Load libraries

library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(colorfindr)
library(httr)
library(scales)

# Load Data

data <- tt_load("2022-11-01")
films <- data %>% pluck(1)

# Explore
films %>% 
  filter(budget != 0 & vote_average != 0) %>% 
  ggplot(aes(budget, vote_average)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  scale_x_log10()

films %>% 
  filter(revenue != 0 & vote_average != 0) %>% 
  ggplot(aes(revenue, vote_average)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  scale_x_log10()

films %>% 
  filter(budget != 0 & revenue != 0) %>% 
  ggplot(aes(budget, revenue)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  scale_x_log10() + 
  scale_y_log10()

films %>% 
  filter(budget != 0 & revenue != 0 & !is.na(revenue_pct) & vote_average != 0) %>% 
  ggplot(aes(revenue_pct, vote_average)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  scale_x_log10()

rev_films <- films %>% 
  mutate(
    revenue_pct = revenue / budget,
    decade = year(release_date) - year(release_date) %% 10,
    decade = fct_reorder(as_factor(decade), release_date)
  ) %>% 
  filter(budget != 0 & revenue != 0 & !is.na(revenue_pct) & vote_average != 0 & vote_count != 0)

rev_films %>% 
  ggplot(aes(release_date, revenue_pct)) + 
  geom_point() +
  geom_smooth(se = TRUE) + 
  geom_hline(yintercept = 1) +
  scale_y_log10(labels = label_number())

rev_films %>% 
  ggplot(aes(vote_average, revenue_pct)) + 
  geom_point() +
  geom_smooth(se = TRUE) + 
  geom_hline(yintercept = 1) +
  scale_y_log10(labels = label_number()) + 
  facet_wrap(~ decade)

rev_films %>% 
  ggplot(aes(vote_count, revenue_pct)) + 
  geom_point() +
  geom_smooth(se = TRUE) + 
  geom_hline(yintercept = 1) +
  scale_y_log10(labels = c(0.01, 0.1, 1, 10, 100, 1000), breaks = c(0.01, 0.1, 1, 10, 100, 1000)) +
  scale_x_log10(labels = label_number()) + 
  facet_wrap(~ decade) 

rev_films_dec <- rev_films %>% 
  group_by(decade) %>% 
  summarise(
    revenue_pct = mean(revenue_pct),
    revenue_pct2 = sum(revenue / sum(budget)),
    vote_average = mean(vote_average)
  )

rev_films_dec %>% 
  ggplot(aes(x = decade)) +
  geom_col(aes(y = revenue_pct2))
  

films %>% 
  mutate(
    decade = year(release_date) - year(release_date) %% 10
  ) %>% 
  count(decade)

films %>% 
  summarise(sum(!is.na(poster_path)))


scrape_colors <- function(url){
  url <- paste0("https://www.themoviedb.org/t/p/w1280", url)
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(pic <- tempfile(fileext = file_ext)))
  
  get_colors(img = pic, top_n = 1) %>% 
  pull(col_hex)  
}

films <- films %>% 
  mutate(
    color = case_when(
      is.na(poster_path) ~ NA,
      TRUE ~ scrape_colors(poster_path)
    )
  )

films <- films %>% 
  filter(!is.na(poster_path)) %>% 
  mutate(
    color = map(.x = poster_path, get_colors(img = paste0("https://www.themoviedb.org/t/p/w1280", poster_path), min_share = 0.01))
  )

films %>% 
  filter(!is.na(poster_path)) %>% 
  mutate(
    ext = str_extract(poster_path, "\\.jpg|\\.jpeg|\\.png|\\.bmp|\\.tif|\\.svg")
  ) %>% 
  count(ext)
