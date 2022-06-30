# Tidying Script - Transportation

#Library Load-in====
library(tidyverse) #For everything data#
library(sf) #For converting dataframes into shapefiles#

#Data Load-in====
transportation_data <- read_csv("data/acs/transportation/Buffalo Tracts ACS Transportation to Work Data - 2015-2019.csv")

#Data transformations===
transportation_data_mod <- transportation_data %>%
  mutate(upper_estimate = estimate + moe,
         lower_estimate = pmax(estimate - moe,0)) %>%
  relocate(lower_estimate, .before = estimate) %>%
  relocate(upper_estimate, .after = estimate) %>%
  select(-c(moe)) %>%
  rename("buffalo_tract" = "tract")



# Writing out the geometry files for use in Tableau/Mapping software==
st_write(transportation_data_mod, "data/acs/transportation/Buffalo Tracts ACS transportation w all est limits 2015 - 2019.shp")

# Writing to a spreadsheet without the geometries
transportation_data_mod_nogeo <- transportation_data_mod %>%
  select(-geometry) 

#Writing data to the directory===
writexl::write_xlsx(transportation_data_mod_nogeo, "data/acs/transportation/Buffalo Tracts ACS transportation w all est limits - No Geo - 2015-2019.xlsx")
