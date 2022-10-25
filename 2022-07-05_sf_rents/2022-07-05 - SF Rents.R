# Load libraries
library(tidyverse)
library(lubridate)
library(broom)
library(tidytuesdayR)

# Import data
tuesdata <- tidytuesdayR::tt_load('2022-07-05')
rent <- tuesdata$rent

# Explore
view(rent)
summary(rent)

ggplot(rent, aes(log10(price))) +
  geom_histogram()

# Wrangle
rent <- rent %>% 
  mutate(
    date = ymd(date),
    nhood = factor(nhood),
    city = factor(city),
    county = factor(county),
    price_log10 = log10(price)
  )

# Visualize

# Rent by bedrooms
ggplot(rent, aes(beds, price_log10)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm")

# Rent by county
rent %>% 
  count(county)

rent %>% 
  group_by(county) %>% 
  summarise(
    mean_price = mean(price)
    ) %>% 
  ggplot(aes(fct_reorder(county, mean_price), mean_price)) +
    geom_col() +
    coord_flip()

# Rent by county and year
rent %>% 
  group_by(year, county) %>% 
  summarise(
    price = mean(price)
  ) %>% 
  ggplot(aes(year, price, color = county)) +
    geom_line()

# number of rents by month
rent %>% 
  ggplot(aes(month(date))) +
    geom_bar()

rent %>% 
  ggplot(aes(yday(date))) +
    geom_freqpoly()

rent %>% 
  count(mday = mday(date)) %>% 
  ggplot(aes(mday, n)) +
    geom_line()

# Model

# logprice by county, year and beds

mdl1 <- lm(data = rent, formula = price_log10 ~ county + year + beds)
glance(mdl1)
mdl2 <- lm(data = rent, formula = price_log10 ~ county * year * beds)
glance(mdl2)
