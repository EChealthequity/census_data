# Tidying Script - Race 

#Library Load-in====
library(tidyverse)

#Data Load-in====
race_data <- read_csv("data/acs/race/Buffalo Tracts ACS Race Data - 2015-2019.csv")

#Data transformations===
race_data_mod <- race_data %>%
  mutate(upper_estimate = estimate + moe,
         lower_estimate = pmax(estimate - moe,0)) %>%
  relocate(lower_estimate, .before = estimate) %>%
  relocate(upper_estimate, .after = estimate) %>%
  select(-c(moe)) %>%
  rename("buffalo_tract" = "tract") %>%
  mutate(race = case_when(str_detect(race,"two") ~ "mixed_race",
                          race == "total" ~ "all_races",
                          TRUE ~ race)) 

estimate_types <- c("upper_estimate", "lower_estimate")
#Brain is too fried, pick up later.

race_estimate <- race_data_mod %>%
  select(buffalo_tract,race,estimate,geometry) 

race_estimates <- map(estimate_types, ~ race_)

