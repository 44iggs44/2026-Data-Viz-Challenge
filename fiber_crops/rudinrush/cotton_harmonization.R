# Project: 2026 data viz challenge
# Created on: 01 Jul 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 01 Jul 2026
# R version 4.5.2

# note:
    # LLM was used for code assistance # nolint: indentation_linter.
    # dataset last downloaded on: 01 July 2026

# assumes:
    # you have run projec_DVC # nolint: indentation_linter.
    # files are downloaded to a specific folder path


# does:
    # cleans data sets downloaded from project_DVC # nolint: indentation_linter.
    # harmonizes with manufacturing data

########################################################################
# - 0 Setup
########################################################################



# data file path
data <- file.path(
    "", "Users", "Izz2", "Library", "CloudStorage",
    "OneDrive-SharedLibraries-WestVirginiaUniversity/Freedom Enyetornye - Fame_fiber"
)

########################################################################
# - 1 load cotton data
########################################################################

# get list of files from OneDrive
data_list <- list.files(
    path = file.path(
        data, "cotton_data","raw"
    ),
    pattern = "\\.csv$",
    full.names = TRUE
)

# create master cotton data table
cotton <- lapply(data_list, fread)

# rename the sub tables to match the file names
names(cotton) <- basename(data_list)


########################################################################
# - 2 load industry data
########################################################################

# create manufacturing dataset
fib_naics <- fread(
    file = file.path(   
            data, "master_data", "fiber_manufacturing_all.csv"
        )
)


########################################################################
# - 3 clean data
########################################################################


########################################################################
# - 3(a) setup
########################################################################

# create location id for the cotton datasets
cotton <- lapply(
    cotton, # selects dataset to apply function to
    function(dt) { #function that creates location identifier
        dt[
           , # no row operations 
           location_id := paste( # location id from state, soil cat, cnty
               state_fips_code, asd_code, county_code, sep = "_"
               )
        ]
    }
)

# subset gin counts from cotton
gin_counts <- cotton[["gin_counts.csv"]]
    # obs == 4483

# subset harvested acres "
hrvstd_acrg <- cotton[["harvested_acres.csv"]]
    # obs == 45817

# subset planted acres "
plntd_acrg <- cotton[["planted_acres.csv"]]
    # obs == 16376

# subset production bales
bales <- cotton[["bales_produced.csv"]]
    # obs == 27468

# subset sales
sales <- cotton[["sales.csv"]]
    # obs == 12582

# subset cotton yield
yield <- cotton[["yield.csv"]]
    # obs == 16369

# drop missing values
bales <- bales[!is.na(Value)]
    # obs == 16492

yield_2 <- yield[!is.na(Value)]
    # obs == 16369 no change

sales_2 <- sales[!is.na(Value)]
    # obs == 10258

plntd_2 <- plntd_acrg[!is.na(Value)]
    # obs == 16376 no change

hrvstd_2 <- hrvstd_acrg[!is.na(Value)]
    # obs == 43414

gin_counts_2 <- gin_counts[!is.na(Value)]
    # obs == 4481


########################################################################
# - 3(b) bales data
########################################################################

# create list of variables to keep for bales
keep_vars_bales <- c(
    "location_id", "year", "upland_bales", "pima_bales", "class_desc", 
    "unit_desc", "state_fips_code", "asd_code", "county_code", "state_alpha",
    "asd_desc", "county_name"
)


# create variables specific to the cotton classes
bales[
    , # no row operations 
    upland_bales := ifelse(
        class_desc == "UPLAND", # selects for the upland class
        Value, # assigns the value when class_desc is true
        NA_real_ # Na for doubles
    )
][
    , # no row operations
    pima_bales := ifelse(
        class_desc == "PIMA", # selects for Pima cotton
        Value,
        NA_real_
    )
][
    , # no row ops
    all_class_bales := ifelse(
        class_desc == "ALL CLASSES", #selects for all classes
        Value,
        NA_real_
    )
]

# drop observations without the year reference period
bales <- bales[reference_period_desc == "YEAR"]
    # obs == 16365

# create full data.table with select variables from bales
bales_clean <- bales[
    , # no row operations
    ..keep_vars_bales
]

# ########################################################################
# # - 3(c) gin_counts data
# ########################################################################
# 
# # create list of variables to keep
# keep_vars_gins <- c(
#     "location_id",  "class_desc", 
#     "unit_desc", "state_fips_code", "asd_code", "county_code", "state_alpha",
#     "asd_desc", "county_name"
# )
# 
# # drop values without monthly reference period
# gin_counts_2 <- gin_counts_2[
#     freq_desc == "MONTHLY" # drops observations that arent monthly
# ]
#     # obs == 3777
# 
# # create variables for number of bales
# gin_test <- gin_counts_2[
#     
# ]


########################################################################
# - 3(d) plntd data
########################################################################

keep_vars_plntd <- c(
    "location_id", "year", "upland_acres", "pima_acres", "class_desc", 
    "unit_desc", "state_fips_code", "asd_code", "county_code", "state_alpha",
    "asd_desc", "county_name"
)

# create variables for pima and upland acres
plntd_2[
        , # no row operations 
        upland_acres := ifelse(
            class_desc == "UPLAND", # selects for the upland class
            Value, # assigns the value when class_desc is true
            NA_real_ # Na for doubles
        )
    ][
        , # no row operations
        pima_acres := ifelse(
            class_desc == "PIMA", # selects for Pima cotton
            Value,
            NA_real_
        )
]

# create clean dataset with plntd dat and specific variables
plntd_clean <- plntd_2[
    , # no row operations
    ..keep_vars_plntd
]

# keep only unique values
plntd_clean <- unique(plntd_clean)
    # obs == 16376

########################################################################
# - 3(d) harvested data
########################################################################

# keep vars for harvested data
keep_vars_hrvstd <- c(
    "location_id", "year", "upland_hvst_acr", "pima_hvst_acr", "all_hvst_acr", 
    "class_desc", "unit_desc", "state_fips_code", "asd_code", "county_code",
    "state_alpha", "asd_desc", "county_name"
)

# drop demographic info
hrvstd_2 <- hrvstd_2[
    sector_desc == "CROPS"
][unit_desc == "ACRES" ]
    # obs == 21790

# create variables specific to the cotton classes
hrvstd_2[
    , # no row operations 
    upland_hvst_acr := ifelse(
        class_desc == "UPLAND", # selects for the upland class
        Value, # assigns the value when class_desc is true
        NA_real_ # Na for doubles
    )
][
    , # no row operations
    pima_hvst_acr := ifelse(
        class_desc == "PIMA", # selects for Pima cotton
        Value,
        NA_real_
    )
][
    , # no row ops
    all_hvst_acr := ifelse(
        class_desc == "ALL CLASSES", #selects for all classes
        Value,
        NA_real_
    )
]

# select variables for use
hrvstd_clean <- hrvstd_2[
    , # no row ops
    ..keep_vars_hrvstd
]

# drop remaining duplicates
hrvstd_clean <- unique(hrvstd_clean)
    # obs == 21790


########################################################################
# - 3(e) sales data
########################################################################

# create list of sales variables to keep
keep_vars_sales <- c(
    "location_id", "year", "operations", "sales", "class_desc", "unit_desc", 
    "state_fips_code", "asd_code", "county_code", "state_alpha", "asd_desc", 
    "county_name"
)

# drop demographic data
sales_2 <- sales_2[
    sector_desc == "CROPS"
]
    # obs == 6622


# create variables specific to operations or sales
sales_2[
    , # no row operations 
    sales := ifelse(
        unit_desc == "$", # selects for the sales 
        Value, # assigns the value when unit_desc is true
        NA_real_ # Na for doubles
    )
][
    , # no row operations
    operations := ifelse(
        unit_desc == "OPERATIONS", # selects for count of operatiosn
        Value,
        NA_real_
    )
]

# select variables for use
sales_clean <- sales_2[
    , # no row ops
    ..keep_vars_sales
]

# drop remaining duplicates
sales_clean <- unique(sales_clean)
# obs == 6622


########################################################################
# - 3(f) yield data
########################################################################

# variables to keep
keep_vars_yield <- c(
    "location_id", "year", "upland_yield", "pima_yield", "class_desc", 
    "unit_desc", "state_fips_code", "asd_code", "county_code", "state_alpha",
    "asd_desc", "county_name"
)

# create variables for pima and upland acres
yield_2[
    , # no row operations 
    upland_yield := ifelse(
        class_desc == "UPLAND", # selects for the upland class
        Value, # assigns the value when class_desc is true
        NA_real_ # Na for doubles
    )
][
    , # no row operations
    pima_yield := ifelse(
        class_desc == "PIMA", # selects for Pima cotton
        Value,
        NA_real_
    )
]

# create clean dataset with plntd dat and specific variables
yield_clean <- yield_2[
    , # no row operations
    ..keep_vars_yield
]

# drop duplicate observations if any
yield_clean <- unique(yield_2)
    # obs == 16,369


########################################################################
# - 4 cotton master dataset
########################################################################


location_years <- unique(
    na.omit(
        rbindlist(
            lapply(
                cotton, 
                function(dt) {
                    dt[
                    , 
                    .(location_id, year, state_fips_code, asd_code, county_code)
                    ]
                }
            )
        )
    )
)




cotton_clean <- location_years[hrvstd_clean[, .(location_id, year, state_fips_code, asd_code, county_code, upland_hvst_acr, pima_hvst_acr)],
                             on = .(location_id, year, state_fips_code, asd_code, county_code)]
# obs == 21790

# test mega join
cotton_clean <- cotton_clean[bales_clean[, .(location_id, year, state_fips_code, asd_code, county_code,upland_bales,pima_bales)],
                             on = .(location_id, year, state_fips_code, asd_code, county_code)]
    # obs == 21164

cotton_clean <- cotton_clean[plntd_clean[, .(location_id, year, state_fips_code, asd_code, county_code,pima_acres, upland_acres)],
                             on = .(location_id, year, state_fips_code, asd_code, county_code)]
    # obs == 355532

cotton_clean <- cotton_clean[sales_clean[, .(location_id, year, state_fips_code, asd_code, county_code,sales, operations)],
                             on = .(location_id, year, state_fips_code, asd_code, county_code)]
    # obs == 38866

cotton_clean <- cotton_clean[yield_clean[, .(location_id, year,state_fips_code, asd_code, county_code, upland_yield, pima_yield)],
                             on = .(location_id, year, state_fips_code, asd_code, county_code)]
    ## obs == 53681

target_cols <- c(
    "upland_hvst_acr", "pima_hvst_acr", "upland_bales", "pima_bales",
    "pima_acres", "upland_acres", "sales", "operations",
    "upland_yield", "pima_yield"
)

cotton_max <- cotton_clean[, lapply(.SD, max, na.rm = TRUE),
                           by = .(location_id, year),
                           .SDcols = target_cols]
