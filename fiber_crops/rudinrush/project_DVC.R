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
        end = 2 # select the first two characters
        ) 
][, # no row operations
    county_fips := str_sub( #creates string of county fips codes
        area_fips, # character variable
        start = 3, # starts at 3rd character inclusive
        end = 5 # ends at last character inclusive
    )
]

# creates data table of fips codes for matching 
tig_lookup <- unique(as.data.table(fips_codes)[, # no row operations 
    .(state_code, state) # matches fips codes to state abbreviatiosn
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
    .(state_code, county_code, county) # selects specific variables
    ]
)

# create variable of county names for major data
fbr_cnty_lvl <- fbr_cnty_lvl[
    tig_lookup,
    county := i.county, # maps fips code to county 
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
# - 2 download crop data
########################################################################


########################################################################
# - 2(a) nass parameter lists
########################################################################

# get parameters for query
cttn_param <- list(
    commodity_desc = "COTTON",
    agg_level_desc = "COUNTY",
    prodn_practice_desc = "ALL PRODUCTION PRACTICES",
    year = 1990:2025
)

# create parameters list for gins and ginned bales
gin_param <- c(
    cttn_param, list(
        statisticcat_desc = "ACTIVE GINS"
    )
)

# parameters for harvested acres query
hvstd_param <- c(
    cttn_param, list(
        statisticcat_desc = "AREA HARVESTED"
    )
)

# get parameters for planted cotton acreage query
plntd_param <- c(
    cttn_param, list(
        statisticcat_desc = "AREA PLANTED"
    )
)

# parameters for cotton production query
prod_param <- c(
    cttn_param, list(
        statisticcat_desc = "PRODUCTION",
        unit_desc = "480 LB BALES"
    )
)

# create parameters for yields  query
yield_param <- c(
    cttn_param, list(
        statisticcat_desc = "YIELD",
        unit_desc = "LB / ACRE"
    )
)

# create parameters for sales query
sales_param <- c(
    cttn_param, list(
        statisticcat_desc = "SALES"
    )
)

########################################################################
# - 2(b) download datasets
########################################################################

# create data table of cotton gins
gin_counts <- as.data.table(rnassqs::nassqs(gin_param))
    # *** obs == 4483

# pull harvested acres data fromm  NASS
hvstd_acrg <- as.data.table(rnassqs::nassqs(hvstd_param))
    # *** obs == 45,817

# pull planted acres data from NASS
plntd_acrg <- as.data.table(rnassqs::nassqs(plntd_param))
    # ***obs == 16,376

# pull production data from nass
prod_bale <- as.data.table(rnassqs::nassqs(prod_param))
    # *** obs == 27,468    

# pull sales data from nass
sales <- as.data.table(rnassqs::nassqs(sales_param))
    # *** obs == 12,582

# create data table of yields
yield <- as.data.table(rnassqs::nassqs(yield_param))
    # *** obs == 16,369


########################################################################
# - 3 clean data
########################################################################

# clean harvested acreage 
hvstd_clean <- hvstd_acrg[
    , # no row operations
    `:=`(
        pima_acres = fcase(
            class_desc == "PIMA" & unit_desc == "ACRES", # creates variable of acres of pima per county
            Value, default = NA
        ),
        upland_acres = fcase(
            class_desc == "UPLAND" & unit_desc == "ACRES", # creates variable of acres of upland per county
            Value, default = NA
        ),
        all_acres = fcase(
            class_desc == "ALL CLASSES" & unit_desc == "ACRES", # creates variable of acres of all classes per county
            Value, default = NA
        ),
        pima_ops = fcase(
            class_desc == "PIMA" & unit_desc == "OPERATIONS", # creates variable of operations with pima per county
            Value, default = NA
        ),
        upland_ops = fcase(
            class_desc == "UPLAND" & unit_desc == "OPERATIONS", # creates variable of ops with upland "
            Value, default = NA
        ),
        all_ops = fcase(
            class_desc == "ALL CLASSES" & unit_desc == "OPERATIONS", # creates variable of operations of all classes per county
            Value, default = NA
        )
    )
]

# clean planted area data
# drop observations that arent year reference
#plntd_test <- plntd_acrg[reference_period_desc == "YEAR"]
    # obs == 16376 NOTE: THIS CHANGES NOTHING

# create variables for planted pima acres and planted upland acres
plntd_clean <- plntd_acrg[
    , # no row operations
    `:=`(
        pima_acres = fcase(
            class_desc == "PIMA", # value only when pima cotton
            Value, # populates new variable
            default = NA # if missing then NA
        ),
        upland_acres = fcase(
            class_desc == "UPLAND", # value only when pima cotton
            Value, # populates new variable
            default = NA # if missing then NA
        )
    )
]

# clean bales prdctn data
# drop observations that aren't year reference
prod_bale <- prod_bale[reference_period_desc == "YEAR"]
    # obs == 16365

# create variables for specific number of bales for each category
prod_clean <- prod_bale[
    , #no row operations
    `:=`(
        pima_bales = fcase(
            short_desc == "COTTON, PIMA - PRODUCTION, MEASURED IN 480 LB BALES",
            Value, default = NA
        ),
        upland_bales = fcase(
            short_desc == "COTTON, UPLAND - PRODUCTION, MEASURED IN 480 LB BALES",
            Value, default = NA
        ),
        all_bales = fcase(
            short_desc == "COTTON - PRODUCTION, MEASURED IN 480 LB BALES",
            Value, default = NA
        )
    )
]


# create variables for sales of cotton seed & lint/operations w/ sales
sales_clean <- sales[
    , # no row operations
    `:=`(
        sales = fcase(
            unit_desc == "$", # value only when pima cotton
            Value, # populates new variable
            default = NA # if missing then NA
        ),
        operations = fcase(
            unit_desc == "OPERATIONS", # value only when pima cotton
            Value, # populates new variable
            default = NA # if missing then NA
        )
    )
]

# create variables for yield
yield_clean <- yield[
    , # no row operations
    `:=`(
        pima_yield = fcase(
            class_desc == "PIMA", # value only when pima cotton
            Value, # populates new variable
            default = NA # if missing then NA
        ),
        upland_yield = fcase(
            class_desc == "UPLAND", # value only when pima cotton
            Value, # populates new variable
            default = NA # if missing then NA
        )
    )
]






# craft new dataset with specific variables

# keep unique ones
all_fips <- fbr_cnty_lvl[
    , # no row operations
    .(state_fips, county_fips, year)
]

all_unq <- unique(all_fips)

# rename variables to match
setnames(
    all_unq,
    old = c("state_fips", "county_fips"),
    new = c("state_fips_code", "county_code")
)


# test aggregate
plntd_agg <- plntd_acrg[
    , 
    .(
    pma_plntd_acrs = sum(pima_acres, na.rm = TRUE),
    uplnd_plntd_acrs = sum(upland_acres, na.rm = TRUE)
    ), 
    by = .(state_fips_code, county_code, year)
]

test_test <- plntd_agg[
    all_unq,
    on = .(state_fips_code, county_code, year)
]


dataset_list <- list(all_unq, gin_counts, hvstd_acrg, plntd_acrg, prod_bale, sales, yield)

id_only_list <- lapply(
    dataset_list,
    function(df) {
    setDT(df)
    df[, .(state_fips_code, county_code, year)]
    }
)

master_id <- rbindlist(
    id_only_list,
    use.names = TRUE,
    fill = TRUE
)

mstr_id <- unique(master_id)


# matches nass data
all_unq$year <- as.character(all_unq$year)

all_unq <- mstr_id

# start adding new variables
all_unq[
    gin_counts, # add number of cotton gins from gin data
    on = .(state_fips_code, county_code, year),
    gin_count := i.Value
]

test_test <- all_unq[
    plntd_acrg,
    on = .(state_fips_code, county_code, year),
    `:=`(
        pma_plntd_acrs = i.pima_acres,
        uplnd_plntd_acrs = i.upland_acres
    ),
]







# create list of variables to keep
keep_vars <- c(
    "source_desc", "commodity_desc", "class_desc", "prodn_practice_desc",
    "statisticcat_desc", "unit_desc", "short_desc", "domain_desc",
    "agg_level_desc", "state_fips_code", "state_alpha", "county_code", "county_name",
    "year", "freq_desc", "Value", "CV (%)"
)

# cttn_yld <- unique(cttn_yield, by = keep_vars)
# 
# diff_look <- setdiff(cttn_yield, cttn_yld)
# 

########################################################################
# - 3 create maps and graphs
########################################################################

# group variables and output yearly totals by state, class, and totals
cttn_map_vars <- cttn_acrg[
    class_desc == "PIMA", # select pima cotton
    total_pima := sum(Value),
    by = .(state_fips_code, class_desc, year)
][
    class_desc == "UPLAND",
    total_upland := sum(Value),
    by = .(state_fips_code, class_desc, year)
][
    , # no row operations
    total_cttn := sum(Value),
    by = .(state_fips_code, year)
]

# create line graph of planted acres over time
acrg_grph <- ggplot(
    cttn_map_vars, # data frame for map
    aes(
        x = year,
        y = total_pima,
        color = state_alpha
    )
) +
    geom_line(linewidth = 1.1) +
    facet_wrap(~state_name) +
    theme_minimal() +
    labs(
        title = "Total Acres Pima Cotton Over Time by State",
        x = "Year",
        y = "Pima Cotton Acres"
)

# 1. Get a sorted list of unique years in your data
all_years <- sort(unique(cttn_map_vars$year))

# 2. Select every 5th year for the labels
every_5_years <- all_years[seq(1, length(all_years), by = 5)]


ggplot( cttn_map_vars, # data frame for map
                     aes( x = year, 
                          y = total_cttn, 
                          color = state_alpha,
                          group = state_alpha ) ) + 
    geom_line(linewidth = 1.1) + 
    facet_wrap(~state_name) + 
    theme_minimal() + 
    scale_x_discrete(breaks = every_5_years) + 
    labs( 
        title = "Total Acres Cotton Over Time by State", 
        x = "Year", 
        y = "Cotton Acres" 
)

pima_data <- cttn_map_vars[!is.na(total_pima)]

ggplot( pima_data, # data frame for map
        aes( x = year, 
             y = total_pima, 
             color = state_alpha,
             group = state_alpha ) ) + 
    geom_line(linewidth = 1.1) + 
    facet_wrap(~state_name) + 
    theme_minimal() + 
    scale_x_discrete(breaks = every_5_years) + 
    labs( 
        title = "Total Acres Cotton Over Time by State", 
        x = "Year", 
        y = "Cotton Acres" 
    )


########################################################################
# - n clean up and save
########################################################################


# writ out csv for break time and let others use data
fwrite(
    gin_counts,
    file.path(data,"cotton_data","gin_counts.csv")
)

fwrite(
    hvstd_acrg,
    file.path(data,"cotton_data","harvested_acres.csv")
)

fwrite(
    plntd_acrg,
    file.path(data,"cotton_data","planted_acres.csv")
)

fwrite(
    prod_bale,
    file.path(data,"cotton_data","bales_produced.csv")
)

fwrite(
    sales,
    file.path(data,"cotton_data","sales.csv")
)

fwrite(
    yield,
    file.path(data,"cotton_data","yield.csv")
)



