# Project: 2026 data viz challenge
# Created on: 13 July 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 15 Jul 2026
# R version 4.5.2

# note:
  # maps
  # llm was used for code assistance

# assumes:
  # refined data from data_clean_map file
  # has run project_DVC for packages
  
# does:
  # creates maps that look fun

# to do:
  # everything


########################################################################
# - 0 setup
########################################################################

# # data file path
# data <- file.path(
#   "", "Users", "Izz2", "Library", "CloudStorage",
#   "OneDrive-SharedLibraries-WestVirginiaUniversity/Freedom Enyetornye - Fame_fiber"
# )
# 
# # figures folder
# fig <- file.path(
#   data, "figures"
# ) 


########################################################################
# - 1 county shape files
########################################################################

# get county shapes
us_counties <- counties(cb = TRUE, year = 2024, class = "sf")

# state shapes for outlines
us_state <- states(cb = TRUE, year= 2024, class = "sf") 

# select california and texas outlines
state_shapes <- us_state[
  us_state$GEOID %in% c("06", "48"),
]
# download county data select california and texas for comparison
county_shapes <- us_counties[
  us_counties$STATEFP %in% c("06", "48"), # selects california and texas for comparison, 
]

# create 
county_shapes <- county_shapes |>
  rename(area_fips = GEOID)

# load cotton production data
cttn_pdctn <- fread(
  file = file.path(
    data, "cotton_data", "refined" ,"cotton_prod_use.csv"
  ),
  colClasses = list(character = "area_fips")
) |> as.data.table()


########################################################################
# - 2 create variables for map
########################################################################

# get data set of cotton production and usage for year == 2000
ca_tx_data_2000 <- cttn_pdctn[
  year == 2000 & 
  state_alpha %in% c("CA", "TX") # selects for california and texas data
 ]

# get dataset of cotton production and usage for year == 2020
ca_tx_data_2020 <- cttn_pdctn[
    year == 2020 & 
    state_alpha %in% c("CA", "TX") # selects for california and texas data
] 

# create list of matching location_ids across both datasets
loc_list <- intersect(
    ca_tx_data_2000$location_id,
    ca_tx_data_2020$location_id
)

# get list of var names to rename for differences (make data wide)

# numeric columns and variables to keep
num_cols <- setdiff( # create list of differences between two sets
    names(ca_tx_data)[ # first set is the names of the variables that are numeric
        sapply(ca_tx_data, is.numeric)
    ], c("year", "location_id", "area_fips") # second set is identifying variables
)

# append year value
var_name_2000 <- paste0(
    num_cols,
    "_2000"
)

# drop observations not in loc_list
ca_tx_data_2000<- ca_tx_data_2000[
    location_id %in% loc_list,
]

ca_tx_data_2020 <- ca_tx_data_2020[
    location_id %in% loc_list,
    # no col operations
]

# add first part of widened data set
ca_tx_2000 <- setnames(
    ca_tx_data_2000,
    num_cols,
    var_name_2000
)

# change tail of new var list
var_name_2020 <- paste0(
        num_cols,
        "_2020"
)


# rename 2020 dataset
setnames(
    ca_tx_data_2020,
    num_cols,
    var_name_2020
)

# drop year variable
ca_tx_data_2020[
    , # no row operations
    year := NULL
]

# merge data a together
ca_tx_comp <- ca_tx_data_2000[
    ca_tx_data_2020,
    on = .(location_id)
]

# create differences data
ca_tx_comp <- ca_tx_comp[
    , # no row opeartions
    `:=`(
        up_diff = (upland_bales_2020 - upland_bales_2000) / upland_bales_2000,
        pma_diff = (pima_bales_2020 - pima_bales_2000) / pima_bales_2000,
        ttl_diff = (ttl_bales_2020 - ttl_bales_2000) / ttl_bales_2000,
        up_mill_diff = (up_mill_use_1k_ble_2020 - up_mill_use_1k_ble_2000) / 
            up_mill_use_1k_ble_2000,
        pma_mill_diff = (pima_mill_use_1k_ble_2020 - pima_mill_use_1k_ble_2000) /
            pima_mill_use_1k_ble_2000,
        ttl_mill_diff = (mll_use_1k_ble_2020 - mll_use_1k_ble_2000) / 
            mll_use_1k_ble_2000
        
    )
][
    , # no row operations
    `:=`(
        indx_uplnd = up_diff / up_mill_diff,
        indx_pima = pma_diff / pma_mill_diff,
        indx_ttl = ttl_diff / ttl_mill_diff
    )
]


ca_tx_comp_shp <- left_join(
  county_shapes,
  ca_tx_comp,
  by = "area_fips"
)


tx_data <- ca_tx_comp_shp[
  ca_tx_comp_shp$STUSPS == "TX",
  # no col ops
]

ca_data <- ca_tx_comp_shp[
  ca_tx_comp_shp$STUSPS == "CA",
  # no col ops
]


tx_map <- tm_shape(tx_data) +
  tm_polygons(
    fill = "indx_uplnd",
    fill.scale = tm_scale_continuous(),
    fill.legend = tm_legend(
      title = "Change in Upland Prouduction Sensitivity to Domestic Upland Demand",
      orientation = "landscape",
      position = tm_pos_out("center", "top", pos.h = "center")
    )
  )

print(tx_map)

ca_map <- tm_shape(ca_data) + 
  tm_polygons(
    fill = "indx_pima",
    fill.scale = tm_scale_continuous(),
    fill.legend = tm_legend(
      title = "Change in Pima Production Sensitivity to Domestic Pima Demand",
      orientation = "landscape",
      position = tm_pos_out("center", "top", pos.h = "center")
    )
  )

print(ca_map)

ca_tx_comp <- tmap_arrange(ca_map, tx_map, ncol = 2)

print(ca_tx_comp)

tmap_save(
  ca_tx_comp,
  filename = file.path(fig,"ca_tx_prod_to_usage.png"),
  width = 7,
  height = 5,
  dpi = 300
)





tx_ca_pma_map <-
  tm_shape(pma_comp) +
  tm_polygons(
    fill = "pma_chg_wrt_mill_chg",
    stroke.alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 10,
      value.na = "grey75"
    ),
    fill.legend = tm_legend(
      title = "Change in Share of Cotton"
    )
  ) +
  tm_facets( columns = ) + 
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "grey75"
  ) +
  tm_shape(state_shapes) +
  tm_borders(col = "black",
             lwd = 2) +
  tm_options(legend.na.show = FALSE)

print(tx_ca_pma_map)


tmap_save(
  tx_ca_pma_map,
  filename = file.path(fig,"pma_cali_ELS_chg_area.png"),
  width = 7,
  height = 5,
  dpi = 300
)


## END ##