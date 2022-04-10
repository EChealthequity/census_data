#==== Buffalo Census Tracts - ACS Age and Sex Status Data Processing Script====#

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
agesex_code <- acs_codes$code[which(acs_codes$category == "age and sex")]

#tracts are only available in 5 year increments. Set the "start" year===
year <- 2016

# Pulling matching variable names from acs database for all year 2016-2020. Storing in a list==
acsvariables_list <- map(year, ~load_variables(.x, "acs5/subject", cache = TRUE) %>%
                           filter(str_detect(name, agesex_code)))

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
expected_vars <- `names<-`(readRDS("scripts/utilities/acs_agesex_vars_expected.RDS"), NULL)

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
buffalo_tracts_acs_agesex_16_20 <- get_acs(geography = "tract",
                                            state = "NY",
                                            county = "Erie",
                                            year = 2020,
                                            survey = "acs5",
                                            variables = c("total_under_5" = acs_variables$name[2],
                                                          "total_5_9" = acs_variables$name[3],
                                                          "total_10_14" = acs_variables$name[4],
                                                          "total_15_19" = acs_variables$name[5],
                                                          "total_20_24" = acs_variables$name[6],
                                                          "total_25_29" = acs_variables$name[7],
                                                          "total_30_34" = acs_variables$name[8],
                                                          "total_35_39" = acs_variables$name[9],
                                                          "total_40_44" = acs_variables$name[10],
                                                          "total_45_49" = acs_variables$name[11],
                                                          "total_50_54" = acs_variables$name[12],
                                                          "total_55_59" = acs_variables$name[13],
                                                          "total_60_64" = acs_variables$name[14],
                                                          "total_65_69" = acs_variables$name[15],
                                                          "total_70_74" = acs_variables$name[16],
                                                          "total_75_79" = acs_variables$name[17],
                                                          "total_80_84" = acs_variables$name[18],
                                                          "total_85_older" = acs_variables$name[19],
                                                          "total_male" = acs_variables$name[37],
                                                          "total_female" = acs_variables$name[73]),
                                                          cache_table = TRUE,
                                                          geometry = TRUE) %>%
  mutate(NAME = str_remove_all(NAME, "[:alpha:]"),
         NAME = str_trim(NAME),
         NAME = str_remove_all(NAME, ","),
         NAME = str_trim(NAME)) %>%
  select(c(NAME, variable, estimate, moe)) %>%
  rename("age_sex_totals" = "variable",
         "tract" = "NAME")


# Loading in pre-set buffalo tracts designations
buffalo_tracts <- read_csv("scripts/utilities/buffalo_tracts.csv",
                           col_types = cols(tract = col_character())) %>%
  mutate(tract = if_else(tract == "1.1","1.10",tract))

# Filtering out all Erie County tracts for just Buffalo tracts
buffalo_tracts_acs_agesex_16_20  <- buffalo_tracts_acs_agesex_16_20   %>%
  filter(tract %in% buffalo_tracts$tract) %>%
  mutate(tract = as.character(tract))


# Saving the data to the directory====
write_csv(buffalo_tracts_acs_agesex_16_20, "data/acs/poverty/Buffalo Tracts ACS Age Sex Data - 2016-2020.csv")

# Pulling in a custom function to place data into a bucket and up to the cloud===
cloud_saver <- readRDS("../cloud_setup/utilities/cloud_saver.rds")

# Uploading the ACS Race data for Buffalo====
cloud_saver("Buffalo Tracts ACS Age and Sex Data 2016 to 2020", buffalo_tracts_acs_agesex_16_20)
