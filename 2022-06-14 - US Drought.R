# Load libraries
library(tidyverse)

# Import data
tuesdata <- tidytuesdayR::tt_load('2022-06-14')
drought <- tuesdata$drought
drought_fips <- tuesdata$`drought-fips`
income <- read_csv("raw_data_us_income2020.csv",
                   skip = 3,
                   col_names = c("state", "income"))

# Wrangle
income <- income %>% 
  mutate(
    state = str_to_title(state),
    state = str_replace(state, " ", "-")
    )

d1 <- drought %>% 
  select(D0:D4, W0:W4, state) %>% 
  pivot_longer(cols = -state, names_to = "condition", values_to = "values") %>% 
  group_by(state, condition) %>% 
  summarise(
    value = mean(values, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    state = as_factor(str_to_title(state)),
    condition = as_factor(condition),
    #value = case_when(
     # condition %in% c("W0", "W1", "W2", "W3", "W4") ~ -value,
      #TRUE ~ value
    #)
  ) %>% 
  left_join(income, by = "state") %>% 
  mutate(
    income = as.numeric(str_sub(income, 2, -1))
  )
d1

# Plot
ggplot(d1, aes(income, value)) +
  geom_point() + 
  facet_wrap(vars(condition), scales = "free") + 
  geom_smooth(method = "lm")

