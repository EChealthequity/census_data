#==== Erie County - ACS Race Data Processing Script====#

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

# Pulling the "Race" variable helper==
race_code <- acs_codes$code[which(acs_codes$category == "race")]

# Setting the desired years====
years <- 2010:2019

# Pulling matching variable names from acs database for all year 2010-2019. Storing in a list==
acsvariables_list <- map(years, ~load_variables(.x, "acs1", cache = TRUE) %>%
                           filter(str_detect(name, race_code)))

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
expected_vars <- `names<-`(readRDS("scripts/utilities/acs_vars_expected.RDS"), NULL)

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
         label = if_else(str_detect(label, "Two or more racesTwo"),str_remove_all(label, "Two or more races"), label))

# Pulling all place data for New York====
# Filtering for Buffalo
# Only pulling variables we need
ALL_erie_race <- map(years, ~ get_acs(geography = "county",
                                          state = "NY",
                                          county = "Erie",
                                          year = .x,
                                          survey = "acs1",
                                          variables = c("total" = acs_variables$name[1],
                                                        "white" = acs_variables$name[2],
                                                        "black" = acs_variables$name[3],
                                                        "american_indian_alaskan_native" = acs_variables$name[4],
                                                        "asian" = acs_variables$name[5],
                                                        "native_hawaiian_pacific_islander" = acs_variables$name[6],
                                                        "some_other_race" = acs_variables$name[7],
                                                        "two_or_more_races" = acs_variables$name[8],
                                                        "two_races_w_some_other" = acs_variables$name[9],
                                                        "two_race_no_other_three_more" = acs_variables$name[10]),
                                          cache_table = TRUE) %>%
                           select(c(variable, estimate, moe)) %>%
                           rename("race" = "variable")) 

# Affixing names==
names(ALL_erie_race) <- years

# Collapsing all buffalo results into a single data frame====
erie_acs_race_10_19 <- bind_rows(ALL_erie_race, .id = "year")

# Saving the data to the directory====
write_csv(erie_acs_race_10_19, "data/acs/race/Erie ACS Race Data - 2010-2019.csv")

# Pulling in a custom function to place data into a bucket and up to the cloud===
cloud_saver <- readRDS("../cloud_setup/utilities/cloud_saver.rds")

# Uploading the ACS Race data for Buffalo====
cloud_saver("Erie ACS Race Data 2010 to 2019", erie_acs_race_10_19)
