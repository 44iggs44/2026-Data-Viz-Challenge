# Project: 2026 data viz challenge 
# Created on: 23 June 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 01 Jul 2026
# R version 4.5.2

# note: 
    # LLM was used for code assistance
    # dataset last downloaded on: 01 July 2026

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
     "maps", "rnassqs", "arrow", "assertthat"
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


# ########################################################################
# # - 1 load industry data
# ########################################################################
# 
# # create data object 
# fib_naics <- data.table::fread(
#     file = file.path(
#         data, "master_data", "fiber_manufacturing_all.csv")
# )
# 
# # # create data object of one subset of data for data exp
# # test <- data.table::fread(
# #     file = file.path(
# #         data, "master_data", "2012.annual.by_industry",
# #         "2012.annual 3131 Fiber, yarn, and thread mills.csv"
# #     )
# # )
# 
# # get state and county names from data object
# fbr_cnty_lvl <- fib_naics[, # no row operations
#     state_fips := str_sub(
#         area_fips, # create string of state fips codes
#         start = 1, 
#         end = 2 # select the first two characters
#         ) 
# ][, # no row operations
#     county_fips := str_sub( #creates string of county fips codes
#         area_fips, # character variable
#         start = 3, # starts at 3rd character inclusive
#         end = 5 # ends at last character inclusive
#     )
# ]
# 
# # creates data table of fips codes for matching 
# tig_lookup <- unique(as.data.table(fips_codes)[, # no row operations 
#     .(state_code, state) # matches fips codes to state abbreviatiosn
#     ]
# )
# 
# # match state abbreviations to by joining 
# fbr_cnty_lvl <- fbr_cnty_lvl[
#     tig_lookup,
#     state_abb := i.state, # creates state abbreviation variable
#     on = .(state_fips = state_code) # joins when same state fips codes 
# ]
# 
# # create look up for counties
# tig_lookup <- unique(as.data.table(fips_codes)[,
#     .(state_code, county_code, county) # selects specific variables
#     ]
# )
# 
# # create variable of county names for major data
# fbr_cnty_lvl <- fbr_cnty_lvl[
#     tig_lookup,
#     county := i.county, # maps fips code to county 
#     on = .(state_fips = state_code, county_fips = county_code)
# ]
# 
# # sort for convenience in viewer
# setcolorder(
#     fbr_cnty_lvl,
#     c("state_abb", "state_fips", "county", "county_fips", "year")
#     )
# 
# # sort observations by year, state, county, and industry
# setorderv(
#     fbr_cnty_lvl,
#     cols = c("year", "state_fips", "county_fips", "industry_code")
# )
# 
# ########################################################################
# # - 2 download crop data
# ########################################################################
# 
# 
# ########################################################################
# # - 2(a) nass parameter lists
# ########################################################################
# 
# # get parameters for query
# cttn_param <- list(
#     commodity_desc = "COTTON",
#     agg_level_desc = "COUNTY",
#     prodn_practice_desc = "ALL PRODUCTION PRACTICES",
#     year = 1990:2025
# )
# 
# # create parameters list for gins and ginned bales
# gin_param <- c(
#     cttn_param, list(
#         statisticcat_desc = "ACTIVE GINS"
#     )
# )
# 
# # parameters for harvested acres query
# hvstd_param <- c(
#     cttn_param, list(
#         statisticcat_desc = "AREA HARVESTED"
#     )
# )
# 
# # get parameters for planted cotton acreage query
# plntd_param <- c(
#     cttn_param, list(
#         statisticcat_desc = "AREA PLANTED"
#     )
# )
# 
# # parameters for cotton production query
# prod_param <- c(
#     cttn_param, list(
#         statisticcat_desc = "PRODUCTION",
#         unit_desc = "480 LB BALES"
#     )
# )
# 
# # create parameters for yields  query
# yield_param <- c(
#     cttn_param, list(
#         statisticcat_desc = "YIELD",
#         unit_desc = "LB / ACRE"
#     )
# )
# 
# # create parameters for sales query
# sales_param <- c(
#     cttn_param, list(
#         statisticcat_desc = "SALES"
#     )
# )
# 
# ########################################################################
# # - 2(b) download datasets
# ########################################################################
# 
# # create data table of cotton gins
# gin_counts <- as.data.table(rnassqs::nassqs(gin_param))
#     # *** obs == 4483
# 
# # pull harvested acres data fromm  NASS
# hvstd_acrg <- as.data.table(rnassqs::nassqs(hvstd_param))
#     # *** obs == 45,817
# 
# # pull planted acres data from NASS
# plntd_acrg <- as.data.table(rnassqs::nassqs(plntd_param))
#     # ***obs == 16,376
# 
# # pull production data from nass
# prod_bale <- as.data.table(rnassqs::nassqs(prod_param))
#     # *** obs == 27,468    
# 
# # pull sales data from nass
# sales <- as.data.table(rnassqs::nassqs(sales_param))
#     # *** obs == 12,582
# 
# # create data table of yields
# yield <- as.data.table(rnassqs::nassqs(yield_param))
#     # *** obs == 16,369
# 
# 
# ########################################################################
# # - n clean up and save
# ########################################################################
# 
# 
# # writ out csv for break time and let others use data
# fwrite(
#     gin_counts,
#     file.path(data,"cotton_data","raw","gin_counts.csv")
# )
# 
# fwrite(
#     hvstd_acrg,
#     file.path(data,"cotton_data","raw","harvested_acres.csv")
# )
# 
# fwrite(
#     plntd_acrg,
#     file.path(data,"cotton_data", "raw", "planted_acres.csv")
# )
# 
# fwrite(
#     prod_bale,
#     file.path(data,"cotton_data","raw","bales_produced.csv")
# )
# 
# fwrite(
#     sales,
#     file.path(data,"cotton_data","raw","sales.csv")
# )
# 
# fwrite(
#     yield,
#     file.path(data,"cotton_data","raw","yield.csv")
# )
# 
# 
# 
