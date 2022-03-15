#==== Buffalo - ACS Race Data Processing Script====#

# Library Load-in====
library(tidyverse) #For everything data#
library(here) #For easier movement through the directory#
library(tidycensus) #For grabbing census/acs data#

# Setting the API key====
api_key <- readRDS("scripts/utilities/census_api_key.RDS")
census_api_key(api_key)

# Retrieving the variable code====
# Pulling in race variables table===
acs_codes <- read_csv("scripts/utilities/acs_codes.csv")

# Pulling the "Race" variable==
race_code <- acs_codes$code[which(acs_codes$category == "race")]

# Setting the desired years====
years <- 2010:2019

# Need to work on filtering by place - Pick up here.

test2  <- get_acs(geography = "place",
        table = race_code,
        state = "NY",
        year = 2010)
