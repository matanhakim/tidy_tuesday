# Load libraries
library(tidyverse)
library(tidytuesdayR)
library(bakeoff)

# Load Data

data <- tt_load("2022-10-25")
challenges <- data %>% pluck(1)
bakers <- data %>% pluck(2)
ratings <- data %>% pluck(3)
episodes <- data %>% pluck(4)

# Manipulate

bakers <- bakers %>% 
  mutate(
    age_group = as_factor(case_when(
      age <= 25 ~ "-25",
      age <= 30 ~ "26-30",
      age <= 35 ~ "31-35",
      age <= 45 ~ "36-45",
      TRUE ~ "46+"
    )),
    age_group = fct_reorder(age_group, age)
  )

winners_by_age <- bakers %>% 
  group_by(age_group) %>% 
  summarise(
    technical_winners = sum(technical_winner),
    technical_winners_freq = mean(technical_winner),
    technical_top3 = sum(technical_top3),
    technical_top3_freq = mean(technical_top3),
  )

# Visualize

winners_by_age %>% 
  ggplot(aes(age_group, technical_winners_freq, fill = age_group)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(technical_winners_freq, 2)), vjust = -0.5) +
  theme_light() +
  scale_fill_bakeoff(guide = "none") + 
  scale_y_continuous(limits = c(0, 1.2)) +
  labs(
    title = "Younger bakers win more challenges than older bakers",
    subtitle = "Under 25 bakers are the most successful, while bakers over 46 are the worst off",
    caption = "Source: bakeoff.netlify.app      Plot: @MatanHakim",
    x = "Age group (years)",
    y = "Average number of challenge wins within age group"
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Save
ggsave(path = "2022-10-25_bakeoff", filename = "2022-10-25_bakeoff.png", scale = 1.778)
ggsave(path = "plots", filename = "2022-10-25_bakeoff.png", scale = 1.778)

