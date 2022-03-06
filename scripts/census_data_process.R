#==== Buffalo - ACS Race Data Processing Script====#

# Library Load-in====
library(tidyverse) #For everything data#
library(here) #For easier movement through the directory#

# Data Load-in====
#pulling in file paths of loose csvs===
file_directories <- list.files(here("data/buffalo/acs/race"), full.names = TRUE)

#storing all files into a list===
Buffalo_acs_race_data <- lapply(file_directories, function(x) read_csv(x, show_col_types = FALSE))

# Data Validation====
#total count of all datasets==
file_count <- length(Buffalo_acs_race_data)

#grabbing readable file names==
file_names <- list.files(here("data/buffalo/acs/race"))

#total count of all variables in each dataset==
var_counts <- sapply(Buffalo_acs_race_data, function(x) length(names(x)), simplify = TRUE)


#pulling all present variables names==
var_names <- unique(unlist(lapply(Buffalo_acs_race_data, function(x) unique(names(x)))))

#record of all years each dataset is for==
file_years <- str_extract(file_directories,"\\d{4}")

#setting the expected columns as of 3/5/22==
expected_cols <- c("Label (Grouping)", "Buffalo city, New York!!Estimate", "Buffalo city, New York!!Margin of Error")

#Checking to make sure all column counts are as expected in each dataframe==
if(length(unique(var_counts)) != 1){
  abnoromal_set_length <- file_names[which(var_counts != 3)]
  stop(paste0("Expected ",length(expected_cols)," variables in each dataset.\n Check the ",knitr::combine_words(abnormal_set_length)," data sets.\n Expected variables were:\n",knitr::combine_words(expected_cols, before = "'", after = "'", sep = "\n", and = "")))
}

if(sum(var_names == expected_cols) != 3){
  full_names <- lapply(Buffalo_acs_race_data, function(x) unique(names(x)))
  names(full_names) <- file_names
  full_name_check <- lapply(full_names, function(x) sum(x == expected_cols) == var_count)
  abnormal_set_names <- file_names[which(full_name_check == FALSE)]
  stop(paste0("Issue detected in the following dataset(s):\n",
              knitr::combine_words(abnormal_set_names, sep = "\n", and = ""),"\n",
              "Check the variables for the datasets. These variables are different from the expected variables.\n\n",
              "Expected variables were:\n",knitr::combine_words(expected_cols, before = "'", after = "'", sep = "\n", and = "")))
}

#Manipulating the data frames to clean race categories and condense down into one frame===

#Providing cleaner variable names==
col_names <- tibble(new_names = c("race", "estimated_total", "margin_of_error"),
                    old_names = expected_cols)

col_names <- deframe(col_names)

#fixing names and adding a "year" variable==
for(i in 1:length(Buffalo_acs_race_data)){
  Buffalo_acs_race_data[[i]] <- Buffalo_acs_race_data[[i]] %>%
    rename(all_of(col_names)) %>%
    mutate(year = file_years[i],
           race = str_trim(race, side = "both"),
           race = str_remove(race,"alone"),
           race = str_remove(race,":"),
           margin_of_error = parse_number(margin_of_error),
           overall_total = ifelse(race == "Total",estimated_total,NA))
}

# Combining all years into one workable dataset====
Buffalo_acs_race_data_final <- bind_rows(Buffalo_acs_race_data) %>%
  fill(overall_total, .direction = "down") %>%
  filter(race != "Total") %>%
  relocate(overall_total, .after = estimated_total)

Buffalo_acs_race_data_totals <- bind_rows(Buffalo_acs_race_data) %>%
  filter(race == "Total") %>%
  select(-c(race,overall_total))

# figure out placing into buckets and saving back into directory locally

 
