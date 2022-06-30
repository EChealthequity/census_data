# Tidying Script - Population

#Library Load-in====
library(tidyverse) #For everything data#

#Data Load-in====
population_data <- read_csv("data/acs/population/Buffalo Tracts ACS Total Population Data - 2015-2019.csv")

#Data transformations===
population_data_mod <- population_data %>%
  mutate(upper_estimate = estimate + moe,
         lower_estimate = pmax(estimate - moe,0)) %>%
  relocate(lower_estimate, .before = estimate) %>%
  relocate(upper_estimate, .after = estimate) %>%
  select(-c(moe)) %>%
  rename("buffalo_tract" = "tract") 

#Writing data into directory===
writexl::write_xlsx(population_data_mod, "data/acs/population/Buffalo Tracts ACS Population w all est limits - No Geo - 2015-2019.xlsx")
