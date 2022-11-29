# Load libraries

library(tidyverse)
library(tidytuesdayR)
library(scales)
library(ggimage)
library(countrycode)

# Load Data

data <- tt_load("2022-11-29")
games_raw <- data %>% pluck(1)

# Favorites to win
 favs_vec <- c("Brazil",
           "Spain",
           "France",
           "England",
           "Argentina")

 # Manipulate
 
 favs <- str_flatten(favs_vec, collapse = "|")
 games_teams <- games_raw %>% 
   pivot_longer(c(home_team, away_team), names_to = "venue", values_to = "team") %>% 
   mutate(
     result = case_when(
       outcome == "D" ~ "D",
       outcome == "H" & venue == "home_team" ~ "W",
       outcome == "A" & venue == "away_team" ~ "W",
       TRUE ~ "L"
     )
   ) %>% 
   filter(str_detect(team, favs))
 
 games_summary <- games_teams %>% 
   mutate(
     team = if_else(str_detect(team, "Germany"), "Germany", team)
   ) %>% 
   count(year, team, result) %>% 
   pivot_wider(names_from = result, values_from = n, values_fill = 0) %>% 
   mutate(W_pct = W / (L + W + D))
 
 games_summary_tail <- games_summary %>% 
   slice_tail(n = 5)
 
 # Visualize
 # Team colors from https://www.flagcolorcodes.com/
 team_colors <- c("France" = "#002654",
                  "Brazil" = "#009739",
                  "England" = "#CE1124",
                  "Argentina" = "#6CACE4",
                  "Spain" = "#F1BF00")
 
 # Team codes for flags
 team_codes <- tibble(
   team = c("Brazil", "Spain", "France", "Argentina")
 ) %>% 
   mutate(iso2 = countrycode(team, "country.name", "iso2c")) %>% 
   add_row(team = "England", iso2 = "GB-ENG") %>% 
   left_join(games_summary_tail, by = "team") %>% 
   mutate(
     W_pct = case_when(
       team == "Spain" | team == "England" ~ W_pct - 0.05,
       team == "Argentina" ~ W_pct + 0.05,
       TRUE ~ W_pct
     )
   )
 
 # Final plot
 
 games_summary %>% 
   ggplot(aes(year, W_pct, color = team)) +
   theme_bw() +
   #geom_line(size = 2) +
   geom_smooth(size = 2.5, span = 0.4, se = FALSE) +
   geom_flag(data = team_codes, aes(y = W_pct, image = iso2, color = NULL), x = 2020) +
   scale_y_continuous(labels = label_percent()) +
   scale_color_manual(values = team_colors) +
   labs(
     title = "The Road to Qatar 2022",
     subtitle = "The top 5 favorites to win this year's World Cup had\ndifferent histories of winning in the tournament.\nFrance and Brazil had the most recent success, while\nArgentina and Spain look like they have passed their peak.",
     caption = "Source: FIFA World Cup      Plot: @MatanHakim",
     x = "Year",
     y = "Average win percentage"
   ) +
   theme(plot.caption = element_text(hjust = 0))
 
 # Save
 ggsave(path = "2022-11-29_world_cup", filename = "2022-11-29_world_cup.png", scale = 1.778)
 ggsave(path = "plots", filename = "2022-11-29_world_cup.png", scale = 1.778)
 