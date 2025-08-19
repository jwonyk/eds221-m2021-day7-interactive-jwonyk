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

anti_join(animals, sites)

animals %>% 
  filter(!location %in% sites$location)

anti_join(sites, animals)

# Practice with lubridate
my_date <- "03-13-1998"
lubridate::mdy(my_date) # fixed date to ISO 8601

# New format for date
my_date <- "08-Jun-1974"
lubridate::dmy(my_date)

# Another example of different format
my_date <- "19160518"
lubridate::ymd(my_date)

# What happens if we give lubridate that doesn't make sense?
lubridate::mdy("1942-08-30")

lubridate::dmy("09/12/84")

# working with date-times

time <- "2020-08-12 11:18"
time <- ymd_hm(time, tz = "America/Los_Angeles")

# Convert to PDT
with_tz(time, "America/Los_Angeles")

# Extract info from dates
week(time)
year(time)
day(time)

start_time <- Sys.time()
end_time <- Sys.time()
end_time - start_time

# Practice lubridate within a data frame

urchin_counts <- tribble(
  ~date, ~species, ~size_mm,
  "10/3/2020", "purple", 55,
  "10/4/2020", "red", 48,
  "11/17/2020", "red", 67
  )

urchin_counts %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  mutate(year = year(date), 
         month = month(date),
         day = day(date))

day_1 <- lubridate::ymd("2020-01-06")
day_2 <- ymd("2020-05-18")
day_3 <- ymd("2020-05-19")

# Create time interval
time_interval <- interval(day_1, day_2)
time_length(time_interval, "week")
time_length(time_interval, "year")

# Practice with stringr

# str_detect() to detect string patterns
# Returns TRUE / FALSE depending on whether the pattern is detected

my_string <- "Teddy loves eating salmon and socks"

# does the pattern "love exist within the string?
my_string %>% 
  str_detect("pup")

my_string <- c("burrito", "fish taco", "Taco salad")

# Does the vector element contain the pattern "fish"?
my_string %>% 
  str_detect("fish")

# Powerful when combined with dplyr functions
starwars %>% 
  filter(str_detect(name, "Skywalker"))

firewalkers <- starwars %>% 
  mutate(name = str_replace(name, pattern = "Sky", replacement = "Fire"))

# Cleaning up white space
feedback <- c("I ate    some nachos", "Wednesday morning       ")

# Remove the leading, trailing, and duplicate spaces
str_squish(feedback)

# Remove just the leading and trailing spaces
str_trim(feedback)

# Convert cases
str_to_lower(feedback)
str_to_upper(feedback)
str_to_sentence(feedback)
str_to_title(feedback)

# Count matches in a string
str_count(feedback, pattern = "nachos")
