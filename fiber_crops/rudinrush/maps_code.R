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
# - 0 Setup
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
)
    

# merge in shape data
cotton_shape <- left_join(
    county_shapes,
    cotton_df,
    by = join_by(area_fips)
)


########################################################################
# - 2 create variables for maps
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
         yld_plntd_rto = (ttl_bales / ttl_acres_plntd) * 480
    )
]
 
# create


