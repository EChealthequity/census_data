# Tidying Script - Poverty

#Library Load-in====
library(tidyverse) #For everything data#

#Data Load-in====
poverty_data <- read_csv("data/acs/poverty/Buffalo Tracts ACS Poverty Data - 2015-2019.csv")

#Data transformations===
poverty_data_mod <- poverty_data %>%
  mutate(upper_estimate = estimate + moe,
         lower_estimate = pmax(estimate - moe,0)) %>%
  relocate(lower_estimate, .before = estimate) %>%
  relocate(upper_estimate, .after = estimate) %>%
  select(-c(moe)) %>%
  rename("buffalo_tract" = "tract") 

#Writing data into directory===
writexl::write_xlsx(poverty_data_mod, "data/acs/poverty/Buffalo Tracts ACS Poverty w all est limits - No Geo - 2015-2019.xlsx")
