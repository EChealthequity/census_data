# Tidying Script - Disability

#Library Load-in====
library(tidyverse) #For everything data#
library(sf) #For converting dataframes into shapefiles#

#Data Load-in====
disability_data <- read_csv("data/acs/disability/Buffalo Tracts ACS Disability.csv")

#Data transformations===
disability_data_mod <- disability_data %>%
  mutate(upper_estimate = estimate + moe,
         lower_estimate = pmax(estimate - moe,0)) %>%
  relocate(lower_estimate, .before = estimate) %>%
  relocate(upper_estimate, .after = estimate) %>%
  select(-c(moe)) %>%
  rename("buffalo_tract" = "tract") 

# Writing out the geometry files for use in Tableau/Mapping software==
st_write(disability_data_mod , "data/acs/disability/Buffalo Tracts ACS Disability w all est limits 2015 - 2019.shp")

# Writing to a spreadsheet without the geometries
disability_data_mod_nogeo <- disability_data_mod %>%
  select(-geometry) 

writexl::write_xlsx(disability_data_mod_nogeo, "data/acs/disability/Buffalo Tracts ACS Disability w all est limits - No Geo - 2015-2019.xlsx")



