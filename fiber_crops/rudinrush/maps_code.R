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

# full manufacturing
cttn_mftr <- fread(
    file = file.path(
        data, "cotton_data", "refined", "cttn_mftr.csv"
    ),
    colClasses = list(character = "area_fips")
)

# merge in shape data
cotton_shape <- left_join(
    county_shapes,
    cotton_df,
    by = join_by(area_fips)
)

cotton_df <- cotton_df |>
    mutate(
        up_yld_calc_plnt = uplnd_acres_plntd * upland_yield,
        up_yld_calc_hvst = uplnd_acres_hvstd * upland_yield
    ) |>
    mutate(
        up_bale_plnt = up_yld_calc_plnt / 480,
        up_bale_hvst = up_yld_calc_hvst / 480
    ) |>
    mutate(
        yld_plnt_acr = up_yld_calc_hvst 
    )

# create sub data for maps
cotton_map <- cotton_df |>
    group_by(area_fips) |>
    summarise(
        yld_plnt_acr = mean()
    )

