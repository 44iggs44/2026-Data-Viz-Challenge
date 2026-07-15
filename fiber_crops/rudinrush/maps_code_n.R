# Project: 2026 data viz challenge
# Created on: 13 July 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 13 Jul 2026
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
  year %in% 1990:2010 &
  state_alpha %in% c("CA", "TX"), # selects years from 1990 to 2000
  .(
    year,
    state_alpha,
    cnty_ttl_chg = sum(yoy_chg_ttl_acr, na.rm = TRUE),
    cnty_upl_chg = sum(yoy_chg_upl_acr, na.rm = TRUE),
    cnty_pma_chg = sum(yoy_chg_pma_acr, na.rm = TRUE),
    ttl_mills_chg = sum(yoy_chg_ttl_mills, na.rm = TRUE),
    up_mill_chg = sum(yoy_chg_up_mills, na.rm = TRUE),
    pma_mill_chg = sum(yoy_chg_pma_mills, na.rm = TRUE)
  ),
  by = area_fips
][
  , # no row operations
  `:=`(
    pma_chg_wrt_mill_chg= cnty_pma_chg / pma_mill_chg
  )
]


########################################################################
# - 3 maps
########################################################################

# Texas and California, 2000 vs 2010
tx_ca_pma_comp <- cttn_pdctn[
    state_alpha %in% c("TX", "CA"),
    .(
      state_alpha,
      cnty_ttl_chg = sum(yoy_chg_ttl_acr, na.rm = TRUE),
      cnty_upl_chg = sum(yoy_chg_upl_acr, na.rm = TRUE),
      cnty_pma_chg = sum(yoy_chg_pma_acr, na.rm = TRUE),
      ttl_mills_chg = sum(yoy_chg_ttl_mills, na.rm = TRUE),
      up_mill_chg = sum(yoy_chg_up_mills, na.rm = TRUE),
      pma_mill_chg = sum(yoy_chg_pma_mills, na.rm = TRUE)
    ),
    by = area_fips
  ][
    , # no row operations
    `:=`(
      pma_chg_wrt_mill_chg = cnty_pma_chg / pma_mill_chg
      upl_chg_wrt_mill_chg = cnty_upl_chg / upl_mill_chg
    )
  ]


tx_ca_pma_comp_unq <- unique(tx_ca_pma_comp)

tx_ca_pma_comp_unq <- left_join(
  county_shapes,
  tx_ca_pma_comp_unq,
  by = "area_fips"
)

no_na_pma <- tx_ca_pma_comp_unq |>
  filter(!is.na(end_year))

# get list of na values for 1990-2000
na_vals <- tx_ca_pma_comp_unq |>
  filter(is.na(end_year))

# duplicate data set
na_val_2k <- na_vals |>
  mutate(
    end_year = as.character(2000)
  )

na_val_2010 <- na_vals |>
  mutate(
    end_year = as.character(2010)
  )

pma_comp <- bind_rows(no_na_pma, na_val_2k, na_val_2010)

# 

tx_ca_pma_comp_unq$STUSPS <- factor(tx_ca_pma_comp_unq$STUSPS, levels = c("TX", "CA"))
pma_comp$end_year <- factor(pma_comp$end_year, levels = c("2000", "2010"))


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
      title = "Change in Planted Area of ELS Cotton Scaled by The Change in ELS Cotton Usage by Mills"
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







