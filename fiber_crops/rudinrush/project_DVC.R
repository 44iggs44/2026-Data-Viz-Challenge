# Project: 2026 data viz challenge 
# Created on: 23 June 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 23
# R version 4.5.2

# note: LLM was used for code assistance

# assumes:
    
# does:
    # sets up file paths
    # creates necessary data tables

########################################################################
# - 0 Setup
########################################################################

# data file path
data <- file.path(
    "","Users", "Izz2", "Library", "CloudStorage",
    "OneDrive-SharedLibraries-WestVirginiaUniversity/Freedom Enyetornye - Fame_fiber"
)

# list of packages to be downloaded or installed
packages <- c(
     "fst", "lubridate", "dplyr", "sf", "readr", "stringr", "ggplot2", "fixest",
     "did2s", "tmap", "data.table", "haven", "tigris", "RColorBrewer", "Cairo",
     "maps", "rnassqs", "arrow", "asserthat"
)
    
# check for missing packages 
missing <- packages[!(packages %in% rownames(installed.packages()))]    
    
# install missing packages
if (length(missing)) install.packages(missing)

# load packages from library
invisible(lapply(packages, library, character.only = TRUE))

# set nass api
api_code <- read_lines("../usda_api.txt") # reads in nass api code locally
nassqs_auth(api_code) # authenticates key

########################################################################
# - 1 load industry data
########################################################################

# create data object 
fib_naics <- data.table::fread(
    file = file.path(
        data, "master_data", "fiber_manufacturing_all.csv")
)

# # create data object of one subset of data for data exp
# test <- data.table::fread(
#     file = file.path(
#         data, "master_data", "2012.annual.by_industry",
#         "2012.annual 3131 Fiber, yarn, and thread mills.csv"
#     )
# )

# get state and county names from data object
fbr_cnty_lvl <- fib_naics[, # no row operations
    state_fips := str_sub(
        area_fips, # create string of state fips codes
        start = 1, 
        end = 2
        ) # select the first two characters
][, # no row operations
    county_fips := str_sub( #creates string of county fips codes
        area_fips, # character variable
        start = 3, # starts at 3rd character inclusive
        end = 5 # ends at last character inclusive
    )
]

# creates data table of fips codes for matching 
tig_lookup <- unique(as.data.table(fips_codes)[, # no row operations 
    .(state_code, state)
    ]
)

# match state abbreviations to by joining 
fbr_cnty_lvl <- fbr_cnty_lvl[
    tig_lookup,
    state_abb := i.state, # creates state abbreviation variable
    on = .(state_fips = state_code) # joins when same state fips codes 
]

# create look up for counties
tig_lookup <- unique(as.data.table(fips_codes)[,
    .(state_code, county_code, county)
    ]
)

# create variable of county names for major data
fbr_cnty_lvl <- fbr_cnty_lvl[
    tig_lookup,
    county := i.county,
    on = .(state_fips = state_code, county_fips = county_code)
]

# sort for convenience in viewer
setcolorder(
    fbr_cnty_lvl,
    c("state_abb", "state_fips", "county", "county_fips", "year")
    )

# sort observations by year, state, county, and industry
setorderv(
    fbr_cnty_lvl,
    cols = c("year", "state_fips", "county_fips", "industry_code")
)

########################################################################
# - 2 load crop data
########################################################################

# get parameters for query
cttn_param <- list(
    commodity_desc = "COTTON",
    agg_level_desc = "COUNTY",
    year = 1990:2025
)

# get parameters for planted cotton acreage
plntd_param <- c(
    cttn_param, list(
        statisticcat_desc = "AREA PLANTED"
    )
)

# pull planted acres data from NASS
cttn_acrg <- as.data.table(rnassqs::nassqs(acres_param))

# create harvested acres data
hvstd_param <- c(
    cttn_param, list(
        statisticcat_desc = "AREA HARVESTED"
    )
)

# pull harvested acres data fromm  NASS
hvstd_acrg <- as.data.table(rnassqs::nassqs(hvstd_param))

# create parameters list for gins and ginned bales
gin_param <- c(
    cttn_param, list(
        statisticcat_desc = c("ACTIVE GINS", "GINNED BALES")
    )
)

# create data table of cotton gins and ginned bales
gin_counts <- as.data.table(rnassqs::nassqs(gin_param))

# create parameters for 
