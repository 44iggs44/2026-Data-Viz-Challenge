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
    # you have run project_DVC # nolint: indentation_linter.
    # files are downloaded to a specific folder path


# does:
    # cleans data sets downloaded from project_DVC # nolint: indentation_linter.
    # harmonizes with manufacturing data

# to do:
    # none

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

# select identifying variables
loc_vars <- c(
    "location_id", "state_fips_code", "state_alpha", "asd_code", "asd_desc",
    "county_code", "county_name"
)

locations <- cotton |>
    # Explicitly keep only the columns in loc_vars and drop everything else
    map(~ select(.x, any_of(loc_vars))) |>
    bind_rows() |>
    # Get the unique row combinations of those remaining columns
    distinct()

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

yield <- yield[!is.na(Value)]
    # obs == 16369 no change

sales <- sales[!is.na(Value)]
    # obs == 10258

plntd <- plntd_acrg[!is.na(Value)]
    # obs == 16376 no change

hrvstd <- hrvstd_acrg[!is.na(Value)]
    # obs == 43414

gin_counts <- gin_counts[!is.na(Value)]
    # obs == 4481


########################################################################
# - 3(b) bales data
########################################################################

# create cotton specific variables
bales_upland <- bales |>
    # year/location_id is level of observation
    group_by(year, location_id) |>
    # select for upland cotton class
    filter(class_desc == "UPLAND") |>
    # select only annual reference periods
    filter(reference_period_desc == "YEAR") |>
    # select identifying variables and rename value
    select("upland_bales" = Value)

bales_pima <- bales |>
    # year/location_id is level of observation
    group_by(year, location_id) |>
    # select for upland cotton class
    filter(class_desc == "PIMA") |>
    # select only annual reference periods
    filter(reference_period_desc == "YEAR") |>
    # select identifying variables and rename value
    select("pima_bales" = Value)

bales_all <- bales |>
    # year/location_id is level of observation
    group_by(year, location_id) |>
    # select for upland cotton class
    filter(class_desc == "ALL CLASSES") |>
    # select only annual reference periods
    filter(reference_period_desc == "YEAR") |>
    # select identifying variables and rename value
    select("all_bales" = Value) # NOTE: all bales are not measured in year pd

bales_clean <- full_join(
    bales_upland, # parent 
    bales_pima, # child
    by = join_by(location_id, year)
)

########################################################################
# - 3(c) gin_counts data
########################################################################

# select number of gins by county and year
gin_clean <- gin_counts |>
    # group by year and location for id
    group_by(location_id, year) |>
    # filter for monthly observations
    filter(freq_desc == "MONTHLY") |>
    # drop point in time observations
    filter(domain_desc == "TOTAL") |>
    select("active_gins" = Value) # very few obs also short time period unhelpful


# join with cleaned bales data
cotton_df <- full_join(
    bales_clean,
    gin_clean,
    by = join_by(location_id, year)
)

########################################################################
# - 3(d) plntd data
########################################################################

# select upland cotton planted acres
plntd_upland <- plntd |>
    # year/location_id is level of observation
    group_by(year, location_id) |>
    # select for upland cotton class
    filter(class_desc == "UPLAND") |>
    # select only annual reference periods
    filter(reference_period_desc == "YEAR") |>
    # select identifying variables and rename value
    select(location_id, year, "uplnd_acres_plntd" = Value)

# select pima cotton planted acres
plntd_pima <- plntd |>
    # year/location_id is level of observation
    group_by(year, location_id) |>
    # select for pima cotton class
    filter(class_desc == "PIMA") |>
    # select only annual reference periods
    filter(reference_period_desc == "YEAR") |>
    # select identifying variables and rename value
    select(location_id, year, "pima_acres_plntd" = Value)

# create combined clean dataset of planted acres
plntd_clean <- full_join(
    plntd_upland,
    plntd_pima,
    by = join_by(location_id, year)
)

# add new dataset to cotton_df
cotton_df <- full_join(
    cotton_df,
    plntd_clean,
    by = join_by(location_id, year)
)


########################################################################
# - 3(d) harvested data
########################################################################

# select for upland cotton hvstd acres
hvstd_upland <- hrvstd |>
    # year/location_id is level of observation
    group_by(year, location_id) |>
    #select totals
    filter(domain_desc == "TOTAL") |>
    # select for acres
    filter(unit_desc == "ACRES") |>
    # select for upland cotton class
    filter(class_desc == "UPLAND") |>
    # select only annual reference periods
    filter(reference_period_desc == "YEAR") |>
    # select identifying variables and rename value
    select(location_id, year, "uplnd_acres_hvstd" = Value) |>
    slice_max(uplnd_acres_hvstd, n = 1, with_ties = FALSE)

# select for pima cotton hrvstd acres
hvstd_pima <- hrvstd |>
    # year/location_id is level of observation
    group_by(year, location_id) |>
    #select totals
    filter(domain_desc == "TOTAL") |>
    # select for acres
    filter(unit_desc == "ACRES") |>
    # select for upland cotton class
    filter(class_desc == "PIMA") |>
    # select only annual reference periods
    filter(reference_period_desc == "YEAR") |>
    # select identifying variables and rename value
    select(location_id, year, "pima_acres_hvstd" = Value) |>
    # keep only the maximum value per year
    slice_max(pima_acres_hvstd, n = 1, with_ties = FALSE)

# join harvested data
hvstd_clean <- full_join(
    hvstd_upland,
    hvstd_pima,
    by = join_by(location_id, year)
)

# merge into cotton df
cotton_df <- full_join(
    cotton_df,
    hvstd_clean,
    by = join_by(location_id, year)
)


########################################################################
# - 3(e) sales data
########################################################################

# clean sales data
sales_clean <- sales |>
    # group by year and lcoation
    group_by(year, location_id) |>
    # select only totals
    filter(domain_desc == "TOTAL") |>
    # select only $ sales
    filter(unit_desc == "$") |>
    # select only key variables
    select(location_id, year, "sales" = Value)

# merge with cotton total data
cotton_df <- full_join(
    cotton_df,
    sales_clean,
    by = join_by(location_id, year)
)


########################################################################
# - 3(f) yield data
########################################################################

# select upland yield data
yield_upland <- yield |>
    # group by location and year
    group_by(location_id, year) |>
    # select for upland cotton
    filter(class_desc == "UPLAND") |>
    # select only annual values
    filter(freq_desc == "ANNUAL") |>
    # select only key variables
    select(location_id, year, "upland_yield" = Value)

# select pima yield data
yield_pima <- yield |>
    # group by location and year
    group_by(location_id, year) |>
    # select for upland cotton
    filter(class_desc == "PIMA") |>
    # select only annual values
    filter(freq_desc == "ANNUAL") |>
    # select only key variables
    select(location_id, year, "pima_yield" =  Value)

# join yield data
yield_clean <- full_join(
    yield_upland,
    yield_pima,
    by = join_by(location_id, year)
)

# join larger cotton data
cotton_df <- full_join(
    cotton_df,
    yield_clean,
    by = join_by(location_id, year)
)

# location including dataset
cotton_df <- full_join(
    cotton_df,
    locations,
    by = "location_id"
    ) |>
    filter(!is.na(year))


########################################################################
# - 4 merge in manufacturing data
########################################################################

# take cotton dataset and create matching area fips code
cotton_df <- cotton_df |>
    mutate(
        state_fips_code = str_pad(state_fips_code, width = 2, pad = "0"),
        county_code = str_pad(county_code, width = 3, pad = "0"),
        area_fips = paste0(
            state_fips_code, county_code
        )
)


# variables to keep
cotton_merge <- cotton_df |>
    ungroup() |>
    group_by(area_fips, year) |>
    summarise(
        across(
            upland_bales:pima_yield, # value columns
            ~sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
    )

# merge with naics data on area_fips'
cotton_mnfctr <- full_join(
    fib_naics,
    cotton_merge,
    by = join_by(area_fips, year)
)


########################################################################
# - n clean up and save
########################################################################

# writ out csv for break time and let others use data
fwrite(
    gin_counts,
    file.path(data,"cotton_data","refined","gin_counts.csv")
)

fwrite(
    hvstd_clean,
    file.path(data,"cotton_data","refined","harvested_acres_clean.csv")
)

fwrite(
    plntd_clean,
    file.path(data,"cotton_data", "refined", "planted_acres_clean.csv")
)

fwrite(
    bales_clean,
    file.path(data,"cotton_data","refined","bales_clean.csv")
)

fwrite(
    sales_clean,
    file.path(data,"cotton_data","refined","sales_clean.csv")
)

fwrite(
    yield_clean,
    file.path(data,"cotton_data","refined","yield_clean.csv")
)

fwrite(
    cotton_df,
    file.path(data,"cotton_data","refined","cotton_harmonized.csv")
)

fwrite(
    cotton_mnfctr,
    file.path(data, "cotton_data", "refined", "cttn_mftr.csv")
)




# END #









# 
# 
# 
# # drop demographic info
# hrvstd_2 <- hrvstd_2[
#     sector_desc == "CROPS"
# ][unit_desc == "ACRES" ]
#     # obs == 21790
# 
# # create variables specific to the cotton classes
# hrvstd_2[
#     , # no row operations 
#     upland_hvst_acr := ifelse(
#         class_desc == "UPLAND", # selects for the upland class
#         Value, # assigns the value when class_desc is true
#         NA_real_ # Na for doubles
#     )
# ][
#     , # no row operations
#     pima_hvst_acr := ifelse(
#         class_desc == "PIMA", # selects for Pima cotton
#         Value,
#         NA_real_
#     )
# ][
#     , # no row ops
#     all_hvst_acr := ifelse(
#         class_desc == "ALL CLASSES", #selects for all classes
#         Value,
#         NA_real_
#     )
# ]
# 
# # select variables for use
# hrvstd_clean <- hrvstd_2[
#     , # no row ops
#     ..keep_vars_hrvstd
# ]
# 
# # drop remaining duplicates
# hrvstd_clean <- unique(hrvstd_clean)
#     # obs == 21790

# 
# # variables to keep
# keep_vars_yield <- c(
#     "location_id", "year", "upland_yield", "pima_yield", "class_desc", 
#     "unit_desc", "state_fips_code", "asd_code", "county_code", "state_alpha",
#     "asd_desc", "county_name"
# )
# 
# # create variables for pima and upland acres
# yield_2[
#     , # no row operations 
#     upland_yield := ifelse(
#         class_desc == "UPLAND", # selects for the upland class
#         Value, # assigns the value when class_desc is true
#         NA_real_ # Na for doubles
#     )
# ][
#     , # no row operations
#     pima_yield := ifelse(
#         class_desc == "PIMA", # selects for Pima cotton
#         Value,
#         NA_real_
#     )
# ]
# 
# # create clean dataset with plntd dat and specific variables
# yield_clean <- yield_2[
#     , # no row operations
#     ..keep_vars_yield
# ]
# 
# # drop duplicate observations if any
# yield_clean <- unique(yield_2)
#     # obs == 16,369
# 
