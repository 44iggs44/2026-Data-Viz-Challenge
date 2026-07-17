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

# data file path
data <- file.path(
  "", "Users", "Izz2", "Library", "CloudStorage",
  "OneDrive-SharedLibraries-WestVirginiaUniversity/Freedom Enyetornye - Fame_fiber"
)

# figures folder
fig <- file.path(
  data, "figures"
) 


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



# change fips to match cotton and manufacturing data
# cali_shape <- cali_shape |>
#   rename(area_fips = GEOID)
# 
# tx_shape <- tx_shape |>
#   rename(area_fips = GEOID)

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

# looking at total county raw share all acres
ca_tx_data <- cttn_pdctn[
  year %in% 2000:2020 &
  state_alpha %in% c("CA", "TX"), # selects years from 1990 to 2000
  .(
    state_alpha,
    cnty_ttl_chg = sum(yoy_chg_ttl_acr, na.rm = TRUE),
    cnty_upl_chg = sum(yoy_chg_upl_acr, na.rm = TRUE),
    cnty_pma_chg = sum(yoy_chg_pma_acr, na.rm = TRUE),
    ttl_mills_chg = sum(yoy_chg_ttl_mills, na.rm = TRUE),
    up_mill_chg = sum(yoy_chg_up_mills, na.rm = TRUE),
    pma_mill_chg = sum(yoy_chg_pma_mills, na.rm = TRUE)
  ),
  by = .(area_fips, year)
][
  , # no row operations
  `:=`(
    pct_chg_us_ttl_chg = cnty_ttl_chg / sum(cnty_ttl_chg, na.rm = TRUE),
    pct_upl_chg = cnty_upl_chg / sum(cnty_upl_chg, na.rm = TRUE),
    pct_pma_chg = cnty_pma_chg / sum(cnty_pma_chg, na.rm = TRUE),
    pct_upl_mill_chg = up_mill_chg / ttl_mills_chg,
    pct_pma_mill_chg = pma_mill_chg / ttl_mills_chg
  ),
  by = year
][
  , # no row operations
  `:=`(
  chg_upl_wrt_chg_upl_mills = pct_upl_chg / pct_upl_mill_chg,
  chg_pma_wrt_chg_pma_mills = pct_pma_chg / pct_upl_mill_chg
  )
]


########################################################################
# - 3 maps
########################################################################

# Texas and California, 2000 vs 2010
tx_ca_comp <- cttn_pdctn[
  year %in% 2000:2020 &
    state_alpha %in% c("CA", "TX"), # selects years from 1990 to 2000
  .(
    state_alpha,
    cnty_ttl_chg = sum(yoy_chg_ttl_acr, na.rm = TRUE),
    cnty_upl_chg = sum(yoy_chg_upl_acr, na.rm = TRUE),
    cnty_pma_chg = sum(yoy_chg_pma_acr, na.rm = TRUE),
    ttl_mills_chg = sum(yoy_chg_ttl_mills, na.rm = TRUE),
    up_mill_chg = sum(yoy_chg_up_mills, na.rm = TRUE),
    pma_mill_chg = sum(yoy_chg_pma_mills, na.rm = TRUE)
  ),
  by = .(area_fips, year)
][
  , # no row operations
  `:=`(
    pct_chg_us_ttl_chg = cnty_ttl_chg / sum(cnty_ttl_chg, na.rm = TRUE),
    pct_upl_chg = cnty_upl_chg / sum(cnty_upl_chg, na.rm = TRUE),
    pct_pma_chg = cnty_pma_chg / sum(cnty_pma_chg, na.rm = TRUE),
    pct_upl_mill_chg = up_mill_chg / ttl_mills_chg,
    pct_pma_mill_chg = pma_mill_chg / ttl_mills_chg
  ),
  by = year
][
  , # no row operations
  `:=`(
    chg_upl_wrt_chg_upl_mills = pct_upl_chg / pct_upl_mill_chg,
    chg_pma_wrt_chg_pma_mills = pct_pma_chg / pct_upl_mill_chg
  )
]

tx_ca_comp_unq <- unique(tx_ca_comp)

tx_ca_comp_unq <- left_join(
  county_shapes,
  tx_ca_comp_unq,
  by = "area_fips"
)


tx_data <- tx_ca_comp_unq[
  tx_ca_comp_unq$STUSPS == "TX",
  # no col ops
]

ca_data <- tx_ca_comp_unq[
  tx_ca_comp_unq$STUSPS == "CA",
  # no col ops
]


tx_map <- tm_shape(tx_data) +
  tm_polygons(
    fill = "chg_upl_wrt_chg_upl_mills",
    fill.scale = tm_scale_continuous(),
    fill.legend = tm_legend(
      title = "Change in Share of Planted Upland Cotton Acres to Change in Share of Domestic Use",
      orientation = "landscape",
      position = tm_pos_out("center", "top", pos.h = "center")
    )
  )

print(tx_map)

ca_map <- tm_shape(ca_data) + 
  tm_polygons(
    fill = "chg_pma_wrt_chg_pma_mills",
    fill.scale = tm_scale_continuous(),
    fill.legend = tm_legend(
      title = "Change in Share of Planted ELS Cotton Acres to Change in Share of Domestic Use",
      orientation = "landscape",
      position = tm_pos_out("center", "top", pos.h = "center")
    )
  )

print(ca_map)

ca_tx_comp <- tmap_arrange(ca_map, tx_map, ncol = 2)

print(ca_tx_comp)

tmap_save(
  ca_tx_comp,
  filename = file.path(fig,"ca_tx_acres_to_usage.png"),
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







