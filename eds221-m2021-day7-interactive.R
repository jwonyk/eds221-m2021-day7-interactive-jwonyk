# Clearing environment
rm(list = ls())

# Attach packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) # help us work with dates

# Data wrangling refresher
# 1. Only include penguins at Biscoe and Dream Islands
# 2. Remove the year and sex variables
# 3. Add a new column called body_mass_kg with penguin mass converted from grams to kg
# 4. Rename the island variable to location

names(penguins)
view(penguins)

penguins_bnD <- penguins %>% 
  filter(island %in% c("Biscoe", "Dream")) %>% 
  select(-year, -sex) %>% 
  mutate("body_mass_kg" = body_mass_g / 1000) %>% 
  rename(location = island)
  
# 1. Limit to only Adelie penguins
# 2. Remove any observations where flipper_length_mm is NA
# 3. Group the day by sex
# 4. Find the mean, standard deviation, and sample size (n) of flipper lengths for male and females

penguins_Adelie <- penguins %>% 
  filter(species == "Adelie") %>% 
  filter(!is.na(flipper_length_mm)) %>% # remove rows that are not NA
  group_by(sex) %>% 
  summarise(mean = mean(flipper_length_mm),
            standard_dev = sd(flipper_length_mm),
            sample_size = n())

animals <- data.frame(
  stringsAsFactors = FALSE,
  location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
  species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
  maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

# Practice with full_join
# Keeps all rows and adds all columns
full_join(animals, sites)

# left_join()
left_join(animals, sites)

# right_join()
right_join(animals, sites)

# inner_join()
inner_join(animals, sites)

# Filtering joins
semi_join(animals, sites)

animals %>% 
  filter(location %in% sites$location)

anti_join(animals, site)

animals %>% 
  filter(!location %in% sites$location)

