# Project: 2026 data viz challenge
# Created on: 08 July 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 15 Jul 2026
# R version 4.5.2

# note:
    # maps
    # llm was used for code assistance

# assumes:
    # refined data from cotton harmonization
    # has run project_DVC for packages

# does:
    # creates dataset of us cotton production and use
    # writes dataset to file

# to do:
    # complete
    

########################################################################
# - 0 setup
########################################################################

# # data file path
# data <- file.path(
#     "", "Users", "Izz2", "Library", "CloudStorage",
#     "OneDrive-SharedLibraries-WestVirginiaUniversity/Freedom Enyetornye - Fame_fiber"
# )
# 
# # figures folder
# fig <- file.path(
#     data, "figures"
# )

# get county shapes
us_counties <- counties(cb = TRUE, year = 2024, class = "sf")

# download county data
county_shapes <- us_counties[
    !us_counties$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), 
]

# change fips to match cotton and manufacturing data
county_shapes <- county_shapes |>
    rename(area_fips = GEOID)


########################################################################
# - 1 load cotton data
########################################################################

# load cotton data
cotton_df <- fread(
    file = file.path(
        refined, "cotton" ,"cotton_harmonized.csv"
    ),
    colClasses = list(character = "area_fips")
)

# get numbered columns for cotton 
num_cols <- names(cotton_df)[sapply(cotton_df, is.numeric)]

# force numeric columns to have 0s and not NA
setnafill(cotton_df, type = "const", fill = 0, cols = num_cols)

# full manufacturing data
cttn_mftr <- fread(
    file = file.path(
        refined,"cttn_mftr.csv"
    ),
    colClasses = list(character = "area_fips")
)

# cotton demand data
cttn_dmnd <- fread(
    file = file.path( cotton, "ers", "us-cotton-supply-and-demand.csv" )
) |>
    lapply( # forces strings to lowercase for consistency
        function(col) { # applies function to columns
            if (is.character(col)) { # checks for characters in vector
                tolower(col) # function to make all chracters lowercase
            }
            else { # ensures other columns keep data populated
                col
            }
        }
) |>
    setDT() # force object to be data table type

# change nanmes of variables
setnames(cttn_dmnd, "period", "year")

# merge in shape data
cotton_shape <- left_join(
    county_shapes,
    cotton_df,
    by = join_by(area_fips)
)


########################################################################
# - 2 create variables for maps and create full harmonized dataset
########################################################################

# create total county cotton production variables
cotton_df[
    , # row operations
    `:=`(
        ttl_bales = upland_bales + pima_bales,
        ttl_acres_plntd = uplnd_acres_plntd + pima_acres_plntd,
        ttl_acres_hvstd = uplnd_acres_hvstd + pima_acres_hvstd
    )
][
    , # no row operations
    `:=`(ttl_yield = (ttl_bales / ttl_acres_hvstd) * 480, 
         yld_plntd_rto = (ttl_bales / ttl_acres_plntd) * 480 # creates new measure
    )
]
 
# create exports table for 1990 - 2025
cttn_exports <- cttn_dmnd[
    table_number == 1 &
    category == "qty_of_exports" &
    year > 1989,
    .( # names of variables to keep and rename
        year,
        exprts_1k_ble = value
    )
]

# create cotton df with exports
ctn_data <- merge(
    cotton_df,
    cttn_exports,
    by = "year",
    all.x = TRUE
)

# create mill use table
cttn_mills <- cttn_dmnd[
    table_number == 1 &
    category == "mill_use" &
    year > 1989,
    .(
        year,
        mll_use_1k_ble = value
    )
]

# add to ctn data df
ctn_data <- merge(
    ctn_data,
    cttn_mills,
    by = "year",
    all.x = TRUE
)

# create upland exports data
uplnd_exprts <- cttn_dmnd[
    table_number == 2 &
    category == "exports" &
    year > 1989,
    .(
        year,
        up_exprts_1k_ble = value
    )
]

# merge with greater data
ctn_data <- merge(
    ctn_data,
    uplnd_exprts,
    by = "year",
    all.x = TRUE
)

# create upland mill use data set data
uplnd_mills <- cttn_dmnd[
    table_number == 2 &
    category == "mill_use" &
    year > 1989,
    .(
        year,
        up_mill_use_1k_ble = value
    )
]

# merge with cotton data
ctn_data <- merge(
    ctn_data,
    uplnd_mills,
    by = "year",
    all.x = TRUE
)

# create pima exports data
pima_exprts <- cttn_dmnd[
    table_number == 3 &
    category == "exports" &
    year > 1989,
    .(
        year,
        pima_exprts_1k_ble = value
    )
]

# merge with larger data set
ctn_data <- merge(
    ctn_data,
    pima_exprts,
    by = "year",
    all.x = TRUE
)

# create pima exports data
pima_mills <- cttn_dmnd[
    table_number == 3 &
        category == "mill_use" &
        year > 1989,
    .(
        year,
        pima_mill_use_1k_ble = value
    )
]

# merge with larger data set
ctn_data <- merge(
    ctn_data,
    pima_mills,
    by = "year",
    all.x = TRUE
)

# columns to drop
drop <- grep( #search for regex _code
    "_code",
    names(ctn_data), # in ctn data
    value = TRUE
)

# numeric columns
num_col <- setdiff(
    names(ctn_data)[sapply(ctn_data, is.numeric)],
    c("location_id", "area_fips", "year")
)

# drop code numbers for state and cnty
num_col <-setdiff(
    num_col,
    drop
)

# drop non-unique obs keeping the largest values
ctn_data_unq <- ctn_data[
    order(location_id, year), # no row operations
    lapply( # get max values across any non-unique obs with loc and year
        .SD,
        max, 
        na.rm = TRUE
    ),
    by = .(location_id, year) 
]

ctn_data_unq[
    , # no row operations
    lapply(
        .SD,
        base::sum,
        na.rm = TRUE
    ),
    by = .(area_fips, year),
    .SDcols = num_col
][
    , # no row operations
    lapply(
        .SD,
        max,
        na.rm = TRUE
    ),
    by = .(area_fips, year)
    
]

ctn_data_unq <- unique(ctn_data_unq, by = c("area_fips", "year"))


########################################################################
# - 3 create map variables
########################################################################

# # create ratios for maps
# ctn_data_unq[
#     order(area_fips, year), # ensure location year is ordered
#     `:=`(
#         plntd_mills_rto = ttl_acres_plntd / mll_use_1k_ble,
#         hvstd_mills_rto = ttl_acres_hvstd / mll_use_1k_ble,
#         up_plntd_mills_rto = uplnd_acres_plntd / mll_use_1k_ble,
#         up_hvstd_mills_rto = uplnd_acres_hvstd / mll_use_1k_ble,
#         pima_plntd_mills_rto = pima_acres_plntd / mll_use_1k_ble,
#         pima_hvstd_mills_rtso = pima_acres_hvstd / mll_use_1k_ble,
#         plntd_exprt_rto = ttl_acres_plntd / exprts_1k_ble,
#         hvstd_exprt_rto = ttl_acres_hvstd / exprts_1k_ble,
#         up_plntd_exprts_rto = uplnd_acres_plntd / exprts_1k_ble,
#         up_hvstd_exprts_rto = uplnd_acres_hvstd / exprts_1k_ble,
#         pima_plntd_exprts_rto = pima_acres_plntd / exprts_1k_ble,
#         pima_hvstd_exprts_rto = pima_acres_hvstd / exprts_1k_ble,
#         location_id = NULL
#     )
# ]

# create set of numeric columns
cols_alz <- setdiff(
    names(ctn_data_unq)[
        sapply(ctn_data_unq, is.numeric) # checks for numeric cols
    ], c("year", "area_fips")
)


# new names for columns
col_new_name <- paste0("pct_chg_", cols_alz)

# creates new columns of pct change
ctn_data_unq[
    , # no row operations
    (col_new_name) := lapply(
        .SD, function(x) {
            prev <- data.table::shift(x) # creates vector for lag values
            ((x-prev) / prev) * 100 # percent change function
        }
    ),
    by = area_fips, # applies to counties
    .SDcols = cols_alz # selects these columns to apply function to
]

# drop from dataset
ctn_data_unq[
    , # no row ops
    (drop) := NULL # selects all rows in drop to drop
]

# calculate
map_rtos <- ctn_data[
    , # no row opeartions
    .(
        wghtd_plntd_mills = sum(ttl_acres_plntd) / sum(mll_use_1k_ble),
        wghtd_plntd_exprts_rto = sum(ttl_acres_plntd) / sum(exprts_1k_ble)
    ),
    by = area_fips
]

# create raw change variable
ctn_data_unq[
    order(area_fips), #force order
    `:=`(
        yoy_chg_ttl_acr = ttl_acres_plntd - shift(ttl_acres_plntd), # create raw number of change in acres
        yoy_chg_upl_acr = uplnd_acres_plntd - shift(uplnd_acres_plntd), # 
        yoy_chg_pma_acr = pima_acres_plntd - shift(pima_acres_plntd),
        yoy_chg_ttl_mills = mll_use_1k_ble - shift(mll_use_1k_ble),
        yoy_chg_up_mills = up_mill_use_1k_ble - shift(up_mill_use_1k_ble),
        yoy_chg_pma_mills = pima_mill_use_1k_ble - shift(pima_mill_use_1k_ble),
        yoy_chg_ttl_exprt = exprts_1k_ble - shift(exprts_1k_ble),
        yoy_chg_up_exprt = up_exprts_1k_ble - shift(up_exprts_1k_ble),
        yoy_chg_pma_exprt = pima_exprts_1k_ble - shift(pima_exprts_1k_ble)
    ),
    by = area_fips
][
    , # no row operations
    `:=`(
        pct_us_chg_plntd = yoy_chg_ttl_acr / sum(yoy_chg_ttl_acr, na.rm = TRUE),
        pct_us_chg_upl_plntd = yoy_chg_upl_acr / sum(yoy_chg_upl_acr, na.rm = TRUE),
        pct_us_chg_pma_plntd = yoy_chg_pma_acr / sum(yoy_chg_pma_acr, na.rm = TRUE)
    ),
    by = year
]

fwrite(
    ctn_data_unq,
    file = file.path(
        refined, "cotton_prod_use.csv"
    )
)




# 
# # merge to land vars
# map_start <- merge(
#     cotton_shape,
#     map_rtos,
#     all.x = TRUE
# )
# 
# 
# 
# 
# 
# 
# 
# # mpa of mills
# moop <- tm_shape(map_start) +
#     tm_polygons(
#         fill = "wghtd_plntd_mills",
#         style = "quantile",
#         n = 5,
#         title = "County Map of Ratio of Planted Acres to Mills"
#     ) +
#     tm_layout(
#         legend.outside = TRUE,
#         frame = FALSE
#     ) +
#     tm_borders(lwd = 0.1)
# 
# print(moop)
# 
# 
# 
