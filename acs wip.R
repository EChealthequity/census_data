library(tidycensus)

api_key <- "f6a65e8bdde72b184782f0d150e3022c22bb81ce"


census_api_key(api_key)


agesex <- get_acs(geography = "county",
                  county = "Erie",
                 state = "NY",
                 table ="S0101")
