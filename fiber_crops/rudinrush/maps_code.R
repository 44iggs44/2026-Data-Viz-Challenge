# Project: 2026 data viz challenge
# Created on: 08 Jul 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 08 Jul 2026
# R version 4.5.2

# note:
    # maps
    # llm was used for code assistance

# assumes:
    # refined data from cotton harmonization
    # has run project_DVC for packages

# does:
    # creates maps that look fun

# to do:
    # everything
    

########################################################################
# - 0 setup
########################################################################

# data file path
data <- file.path(
    "", "Users", "Izz2", "Library", "CloudStorage",
    "OneDrive-SharedLibraries-WestVirginiaUniversity/Freedom Enyetornye - Fame_fiber"
)

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
        data, "cotton_data", "refined", "cotton_harmonized.csv"
    ),
    colClasses = list(character = "area_fips")
)

# get numbered columns for cotton 
num_cols <- names(cotton_df)[sapply(cotton_df, is.numeric)]

# force numearic columns to have 0s and not NA
setnafill(cotton_df, type = "const", fill = 0, cols = num_cols)

# full manufacturing
cttn_mftr <- fread(
    file = file.path(
        data, "cotton_data", "refined", "cttn_mftr.csv"
    ),
    colClasses = list(character = "area_fips")
)

# cotton demand data
cttn_dmnd <- fread(
    file = file.path(
        data, "cotton_data", "cotton_export_mill_use", 
            "US-Cotton-Supply-and-Demand.csv"
    )
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
        mll_use_1k_ble = value,
        mll_use_pct_chg = 
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


########################################################################
# - 3 create map variables
########################################################################

# create ratios for maps
ctn_data[
    , # no row operations
    `:=`(
        plntd_mills_rto = ttl_acres_plntd / mll_use_1k_ble,
        hvstd_mills_rto = ttl_acres_hvstd / mll_use_1k_ble,
        up_plntd_mills_rto = uplnd_acres_plntd / mll_use_1k_ble,
        up_hvstd_mills_rto = uplnd_acres_hvstd / mll_use_1k_ble,
        pima_plntd_mills_rto = pima_acres_plntd / mll_use_1k_ble,
        pima_hvstd_mills_rtso = pima_acres_hvstd / mll_use_1k_ble,
        plntd_exprt_rto = ttl_acres_plntd / exprts_1k_ble,
        hvstd_exprt_rto = ttl_acres_hvstd / exprts_1k_ble,
        up_plntd_exprts_rto = uplnd_acres_plntd / exprts_1k_ble,
        up_hvstd_exprts_rto = uplnd_acres_hvstd / exprts_1k_ble,
        pima_plntd_exprts_rto = pima_acres_plntd / exprts_1k_ble,
        pima_hvstd_exprts_rto = pima_acres_hvstd / exprts_1k_ble
    )
]

map_rtos <- ctn_data[
    , # no row opeartions
    .(
        wghtd_plntd_mills = sum(ttl_acres_plntd) / sum(mll_use_1k_ble),
        wghtd_plntd_exprts_rto = sum(ttl_acres_plntd) / sum(exprts_1k_ble)
    ),
    by = area_fips
]

# create averages over the years for mapping display
# merge to land vars
map_start <- merge(
    cotton_shape,
    map_rtos,
    all.x = TRUE
)

moop <- tm_shape(map_start) +
    tm_polygons(
        fill = "wghtd_plntd_mills",
        style = "quantile",
        n = 5,
        palette = "YlOrRd",
        title = "County Map of Ratio of Planted Acres to Mills"
    ) +
    tm_layout(
        legend.outside = TRUE,
        frame = FALSE
    ) +
    tm_borders(lwd = 0.5)

print(moop)
