# Tidying Script - Race 

#Library Load-in====
library(tidyverse) #For everything data#
library(sf) #For converting dataframes into shapefiles#

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
  mutate(race = case_when(str_detect(race,"two") ~ "Mixed Race",
                          race == "total" ~ "All Races",
                          TRUE ~ race),
         race = case_when(race == "black" ~ "Black",
                          race == "asian" ~ "Asian",
                          race == "some_other_race" ~ "Some Other Race",
                          race == "american_indian_alaskan_native" ~ "American Indian/Alaskan Native",
                          race == "native_hawaiian_pacific_islander" ~ "Native Hawaiian/Pacific Islander",
                          TRUE ~ race))



# Writing out the geometry files for use in Tableau/Mapping software==
st_write(race_data_mod, "data/acs/race/Buffalo Tracts ACS Race w all est limits 2015 - 2019.shp")

# Writing to a spreadsheet without the geometries
race_data_mod_nogeo <- race_data_mod %>%
  select(-geometry) 

writexl::write_xlsx(race_data_mod_nogeo, "data/acs/race/Buffalo Tracts ACS Race w all est limits - No Geo - 2015-2019.xlsx")





