#==== Buffalo - ACS Race Data Processing Script====#

# Library Load-in====
library(tidyverse) #For everything data#
library(here) #For easier movement through the directory#

# Data Load-in====
#pulling in file paths of loose csvs===
file_directories <- list.files(here("data/acs/age_sex/raw"), full.names = TRUE)

#storing all files into a list===
NY_acs_age_sex_data <- lapply(file_directories, function(x) read_csv(x, show_col_types = FALSE))

# Data Validation====
#total count of all datasets==
file_count <- length(NY_acs_age_sex_data)

#grabbing readable file names==
file_names <- list.files(here("data/acs/age_sex/raw"))

#total count of all variables in each dataset==
var_counts <- sapply(NY_acs_age_sex_data, function(x) length(names(x)), simplify = TRUE)


#pulling all present variables names==
var_names <- unique(unlist(lapply(NY_acs_age_sex_data, function(x) unique(names(x)))))

#record of all years each dataset is for==
file_years <- str_extract(file_directories,"\\d{4}")

#setting the expected columns as of 3/5/22==
expected_cols <- c("Label (Grouping)", "United States!!Estimate", "United States!!Margin of Error", "New York!!Estimate", "New York!!Margin of Error", "Erie County, New York!!Estimate", "Erie County, New York!!Margin of Error", "Buffalo city, New York!!Estimate", "Buffalo city, New York!!Margin of Error")

#Checking to make sure all column counts are as expected in each dataframe==
if(length(unique(var_counts)) != 1){
  abnoromal_set_length <- file_names[which(var_counts != length(expected_cols))]
  stop(paste0("Expected ",length(expected_cols)," variables in each dataset.\n Check the ",knitr::combine_words(abnormal_set_length)," data sets.\n Expected variables were:\n",knitr::combine_words(expected_cols, before = "'", after = "'", sep = "\n", and = "")))
}

if(sum(var_names == expected_cols) != length(expected_cols)){
  full_names <- lapply(NY_acs_age_sex_data, function(x) unique(names(x)))
  names(full_names) <- file_names
  full_name_check <- lapply(full_names, function(x) sum(x == expected_cols) == length(expected_cols))
  abnormal_set_names <- file_names[which(full_name_check == FALSE)]
  stop(paste0("Issue detected in the following dataset(s):\n",
              knitr::combine_words(abnormal_set_names, sep = "\n", and = ""),"\n",
              "Check the variables for the datasets. These variables are different from the expected variables.\n\n",
              "Expected variables were:\n",knitr::combine_words(expected_cols, before = "'", after = "'", sep = "\n", and = "")))
}

#Manipulating the data frames to clean race categories and condense down into one frame===

#Providing cleaner variable names==
col_names <- tibble(new_names = c("race", "us_estimated_total", "us_margin_of_error",
                                  "ny_estimated_total", "ny_margin_of_error", "erie_estimated_total", 
                                  "erie_margin_of_error", "buffalo_estimated_total", "buffalo_margin_of_error"),
                    old_names = expected_cols)

col_names <- deframe(col_names)

#fixing names and adding a "year" variable==
for(i in 1:length(NY_acs_race_data)){
  NY_acs_race_data[[i]] <- NY_acs_race_data[[i]] %>%
    rename(all_of(col_names)) %>%
    mutate(year = file_years[i],
           race = str_trim(race, side = "both"),
           race = str_remove(race,"alone"),
           race = str_remove(race,":"),
           across(ends_with("margin_of_error"), ~ if_else(. == "*****","0",.)),
           across(ends_with("margin_of_error"), ~ parse_number(.)),
           us_overall_total = if_else(race == "Total", us_estimated_total, NA_real_),
           ny_overall_total = if_else(race == "Total", ny_estimated_total, NA_real_),
           erie_overall_total = if_else(race == "Total", erie_estimated_total, NA_real_),
           buffalo_overall_total = if_else(race == "Total", buffalo_estimated_total, NA_real_)) 
}



# Combining all years into one workable dataset====
NY_acs_race_data_final <- bind_rows(NY_acs_race_data) %>%
  group_by(year) %>%
  fill(ends_with("overall_total"), .direction = "down") %>%
    filter(race != "Total") %>%
    relocate(us_overall_total, .after = us_estimated_total) %>%
    relocate(ny_overall_total, .after = ny_estimated_total) %>%
    relocate(erie_overall_total, .after = erie_estimated_total) %>%
    relocate(buffalo_overall_total, .after = buffalo_estimated_total) 

# Pulling out overall totals if needed at some point====
NY_acs_race_data_totals <- bind_rows(NY_acs_race_data) %>%
  filter(race == "Total") %>%
  select(-c(race,contains("overall_total")))

# Pulling in a custom function to place data into a bucket and up to the cloud====
cloud_saver <- readRDS("../cloud_setup/utilities/cloud_saver.rds")

# Uploading the ACS Race data for NY====
cloud_saver("NY ACS Race Data 2010 to 2019", NY_acs_race_data_final)

# Uploading the ACS Race data Totals with proper MOEs for JUST overall totals - no race strats====
cloud_saver("NY ACS Race Total w MOE 2010 to 2019", NY_acs_race_data_totals)

# Saving the clean data locally====
# Cleaned race ACS (2010-2019)==
write_csv(NY_acs_race_data_final, "data/acs/race/clean/ny_acs_race_2010_2019.csv")

# Cleaned overall totals w/ MOE - no strats==
write_csv(NY_acs_race_data_totals, "data/acs/race/clean/ny_acs_race_total_moe.csv")

