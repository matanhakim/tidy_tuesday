# Load libraries
library(tidyverse)
library(tidytuesdayR)

# Import data
tuesdata <- tidytuesdayR::tt_load('2022-06-21')
cnss <- tuesdata$census

# Explore
view(cnss)
summary(cnss)

# Wrangle
cnss <- cnss %>% 
  mutate(
    region = as_factor(region),
    division = as_factor(division),
    year = as_factor(year)
  ) %>% 
  pivot_longer(total:black_slaves, names_to = "group", values_to = "pop") %>% 
  mutate(group = as_factor(group))
view(cnss)
summary(cnss)

# Visualize by population, group and year
cnss %>% 
  filter(region == "USA Total") %>% 
  filter(group %in% c("white", "black_free", "black_slaves")) %>% 
  ggplot(aes(year, pop, color = group, group = group)) +
  geom_line()

# Visualize only black by population, group, year and region
cnss %>% 
  filter(region != "USA Total") %>% 
  filter(group %in% c("black_free", "black_slaves")) %>% 
  group_by(year, group, region) %>% 
  summarise(
    pop = sum(pop)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(year, pop, color = group, group = group)) +
  geom_line() + 
  facet_wrap(~ region, scales = "free")
