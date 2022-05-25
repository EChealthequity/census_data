#==== Buffalo Census Tracts - ACS Means of Transportation to Work Data Processing Script====#

# Library Load-in====
library(tidyverse) #For everything data#
library(here) #For easier movement through the directory#
library(tidycensus) #For grabbing census/acs data#

# Setting the API key====
api_key <- readRDS("scripts/utilities/census_api_key.RDS")
census_api_key(api_key)

# Retrieving the variable code====
# Pulling in acs variables table===
acs_codes <- read_csv("scripts/utilities/acs_codes.csv")

# Pulling the "Age and Sex" variable helper==
transportation_code <- acs_codes$code[which(acs_codes$category == "means of transportation to work")]

#tracts are only available in 5 year increments. Set the "start" year===
year <- 2016

# Pulling matching variable names from acs database for all year 2016-2020. Storing in a list==
acsvariables_list <- map(year, ~load_variables(.x, "acs5", cache = TRUE) %>%
                           filter(str_detect(name, transportation_code)))

# Counting the number of columns in each table, returning the unique values==
acs_col_count <- unique(sapply(acsvariables_list, function(df) length(df)))

# Counting the number of rows in each table, returning the unique values==
acs_row_count <- unique(sapply(acsvariables_list, function(df) nrow(df)))

# Checking if the column and row totals match up across all years====
acs_var_cols <- length(acs_col_count) == 1
acs_var_rows <- length(acs_row_count) == 1

# Adding a catch for if rows and column counts aren't unique across all years====
if (acs_var_cols + acs_var_rows != 2) {
  View(acsvariables_list) 
  stop("The amount of columns and rows in the ACS dataset don't match. Please check the column and row counts in the window that just opened and address the discrepancy.")}


# Adding a catch for if the variable names aren't as expected====
expected_vars <- `names<-`(readRDS("scripts/utilities/acs_transportation_expected.RDS"), NULL)

# Grabbing the unique columns names
unique_vars <- bind_rows(acsvariables_list) %>%
  distinct(name, .keep_all = TRUE) %>%
  select(label) %>%
  unlist()

unique_vars <- `names<-`(unique_vars, NULL)

if (!identical(expected_vars,unique_vars)) {
  cat("Expected Vars:")
  cat("\n")
  print(expected_vars)
  cat("\n")
  cat("Unique Vars:")
  cat("\n")
  print(unique_vars)
  View(acsvariables_list)
  cat("\n")
  stop("A problem was detected in the unique variable names. Please view the output that was sent to the column and manually check the new window that has opened to find the discrepancy.")} else {
    message("Column/Rows amounts and variable name checks have passed validation. Continuing processing...")
  }

#Making a clean reference table for pulling the data===
acs_variables <- bind_rows(acsvariables_list) %>%
  distinct(name, .keep_all = TRUE) %>%
  select(name,label) %>%
  mutate(label = if_else(label == "Estimate!!Total", str_remove(label, "Estimate!!"), str_remove(label, "Estimate!!Total")),
         label = str_remove_all(label, "!!"),
         label = str_replace_all(label, "Estimate", " ")) 

# Pulling all place data for New York====
# Filtering for Buffalo Census Tracts
# Only pulling variables we need
buffalo_tracts_acs_trans_16_20 <- get_acs(geography = "tract",
                                           state = "NY",
                                           county = "Erie",
                                           year = 2020,
                                           survey = "acs5",
                                           variables = c("total - white" = acs_variables$name[1],
                                                         "car/van/truck - white" = acs_variables$name[2],
                                                         "car/van/truck - carpooled white" = acs_variables$name[3],
                                                         "public transportation - white" = acs_variables$name[4],
                                                         "walked - white" = acs_variables$name[5],
                                                         "taxi or other means - white" = acs_variables$name[6],
                                                         "wfh - white" = acs_variables$name[7],
                                                         "total - black" = acs_variables$name[8],
                                                         "car/van/truck - black" = acs_variables$name[9],
                                                         "car/van/truck - carpooled black" = acs_variables$name[10],
                                                         "public transportation - black" = acs_variables$name[11],
                                                         "walked - black" = acs_variables$name[12],
                                                         "taxi or other means - black" = acs_variables$name[13],
                                                         "wfh - black" = acs_variables$name[14],
                                                         "total - american indian/alaska native" = acs_variables$name[15],
                                                         "car/van/truck - american indian/alaska native" = acs_variables$name[16],
                                                         "car/van/truck - carpooled american indian/alaska native" = acs_variables$name[17],
                                                         "public transportation - american indian/alaska native" = acs_variables$name[18],
                                                         "walked - american indian/alaska native" = acs_variables$name[19],
                                                         "taxi or other means - american indian/alaska native" = acs_variables$name[20],
                                                         "wfh - american indian/alaska native" = acs_variables$name[21],
                                                         "total - asian" = acs_variables$name[22],
                                                         "car/van/truck - asian" = acs_variables$name[23],
                                                         "car/van/truck - carpooled asian" = acs_variables$name[24],
                                                         "public transportation - asian" = acs_variables$name[25],
                                                         "walked - asian" = acs_variables$name[26],
                                                         "taxi or other means - asian" = acs_variables$name[27],
                                                         "wfh - asian" = acs_variables$name[28],
                                                         "total - native hawaiian/other pacific" = acs_variables$name[29],
                                                         "car/van/truck - native hawaiian/other pacific" = acs_variables$name[30],
                                                         "car/van/truck - carpooled native hawaiian/other pacific" = acs_variables$name[31],
                                                         "public transportation - native hawaiian/other pacific" = acs_variables$name[32],
                                                         "walked - native hawaiian/other pacific" = acs_variables$name[33],
                                                         "taxi or other means - native hawaiian/other pacific" = acs_variables$name[34],
                                                         "wfh - native hawaiian/other pacific" = acs_variables$name[35],
                                                         "total - some other race" = acs_variables$name[36],
                                                         "car/van/truck - some other race" = acs_variables$name[37],
                                                         "car/van/truck - carpooled some other race" = acs_variables$name[38],
                                                         "public transportation - some other race" = acs_variables$name[39],
                                                         "walked - some other race" = acs_variables$name[40],
                                                         "taxi or other means - some other race" = acs_variables$name[41],
                                                         "wfh - some other race" = acs_variables$name[42],
                                                         "total - mixed race" = acs_variables$name[43],
                                                         "car/van/truck - mixed race" = acs_variables$name[44],
                                                         "car/van/truck - carpooled mixed race" = acs_variables$name[45],
                                                         "public transportation - mixed race" = acs_variables$name[46],
                                                         "walked - mixed race" = acs_variables$name[47],
                                                         "taxi or other means - mixed race" = acs_variables$name[48],
                                                         "wfh - mixed race" = acs_variables$name[49],
                                                         "total - white - not hispanic" = acs_variables$name[50],
                                                         "car/van/truck - white - not hispanic" = acs_variables$name[51],
                                                         "car/van/truck - carpooled white - not hispanic" = acs_variables$name[52],
                                                         "public transportation - white - not hispanic" = acs_variables$name[53],
                                                         "walked - white - not hispanic" = acs_variables$name[54],
                                                         "taxi or other means - white - not hispanic" = acs_variables$name[55],
                                                         "wfh - white - not hispanic" = acs_variables$name[56],
                                                         "total - hispanic/latino" = acs_variables$name[57],
                                                         "car/van/truck - hispanic/latino" = acs_variables$name[58],
                                                         "car/van/truck - carpooled hispanic/latino" = acs_variables$name[59],
                                                         "public transportation - hispanic/latino" = acs_variables$name[60],
                                                         "walked - hispanic/latino" = acs_variables$name[61],
                                                         "taxi or other means - hispanic/latino" = acs_variables$name[62],
                                                         "wfh - hispanic/latino" = acs_variables$name[63]),
                                           cache_table = TRUE,
                                           geometry = TRUE) %>%
  mutate(NAME = str_remove_all(NAME, "[:alpha:]"),
         NAME = str_trim(NAME),
         NAME = str_remove_all(NAME, ","),
         NAME = str_trim(NAME)) %>%
  select(c(NAME, variable, estimate, moe)) %>%
  rename("trans_totals" = "variable",
         "tract" = "NAME")


# Loading in pre-set buffalo tracts designations
buffalo_tracts <- read_csv("scripts/utilities/buffalo_tracts.csv",
                           col_types = cols(tract = col_character())) %>%
  mutate(tract = if_else(tract == "1.1","1.10",tract))

# Filtering out all Erie County tracts for just Buffalo tracts
buffalo_tracts_acs_trans_16_20  <- buffalo_tracts_acs_trans_16_20   %>%
  filter(tract %in% buffalo_tracts$tract) %>%
  mutate(tract = as.character(tract))


# Saving the data to the directory====
write_csv(buffalo_tracts_acs_trans_16_20, "data/acs/poverty/Buffalo Tracts ACS Transportation to Work Data - 2016-2020.csv")

# Pulling in a custom function to place data into a bucket and up to the cloud===
cloud_saver <- readRDS("../cloud_setup/utilities/cloud_saver.rds")

# Uploading the ACS Race data for Buffalo====
cloud_saver("Buffalo Tracts ACS Transportation to Work Data 2016 to 2020", buffalo_tracts_acs_trans_16_20)

