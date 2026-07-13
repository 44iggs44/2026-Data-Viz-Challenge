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

# download county data select california and texas for comparison
county_shapes <- us_counties[
  us_counties$STATEFP %in% c(
    "06", "48" # selects california and texas for comparison
  ), 
]

# change fips to match cotton and manufacturing data
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
map_ntl_share_2000 <- cttn_pdctn[
  year %in% 1990:2000 &
  state_alpha == "CA" |
  state_alpha == "TX", # selects years from 1990 to 2000
  .(
    year,
    cnty_ttl_chg = sum(yoy_chg_ttl_acr, na.rm = TRUE),
    cnty_upl_chg = sum(yoy_chg_upl_acr, na.rm = TRUE),
    cnty_pma_chg = sum(yoy_chg_pma_acr, na.rm = TRUE),
    ttl_mills_chg = sum(yoy_chg_ttl_mills, na.rm = TRUE),
    up_mill_chg = sum(yoy_chg_up_mills, na.rm = TRUE),
    pma_mill_chg = sum(yoy_chg_pma_mills, na.rm = TRUE),
    ttl_exprt_chg = sum(yoy_chg_ttl_exprt, na.rm = TRUE),
    up_exprt_chg = sum(yoy_chg_up_exprt, na.rm = TRUE),
    pma_exprt_chg = sum(yoy_chg_pma_exprt, na.rm = TRUE)
  ),
  by = area_fips
][
  , # no row operations
  `:=`(
    pct_us_ttl_chg = cnty_ttl_chg / sum(cnty_ttl_chg, na.rm = TRUE),
    pct_us_upl_chg = cnty_upl_chg / sum(cnty_upl_chg, na.rm = TRUE),
    pct_us_pma_chg = cnty_pma_chg / sum(cnty_pma_chg, na.rm = TRUE),
    ttl_chg_wrt_mill = cnty_ttl_chg / ttl_mills_chg,
    up_chg_wrt_mill = cnty_upl_chg / up_mill_chg,
    pma_chg_wrt_mill_chg_00 = cnty_pma_chg / pma_mill_chg, # target variable relative change in acres contributed to us scaled by change in number of mills
    ttl_chg_wrt_exprt = cnty_ttl_chg / ttl_exprt_chg,
    up_chg_wrt_exprt = cnty_upl_chg / ttl_exprt_chg,
    pma_chg_wrt_exprt = cnty_pma_chg / ttl_exprt_chg,
    up_chg_wrt_up_exprt = cnty_upl_chg / up_exprt_chg,
    pma_chg_wrt_pma_exprt = cnty_pma_chg / pma_exprt_chg
  )
][
  pct_us_pma_chg == 0,
  `:=`(
    pct_us_pma_chg = NA,
    pma_chg_wrt_mills = NA,
    pma_chg_wrt_pma_exprt = NA
  )
]

# create dataset of values from 2001-2010 for map specifics
map_ntl_share_2010 <- cttn_pdctn[
  year %in% 2001:2010 &
  state_alpha == "CA" |
  state_alpha == "TX", # selects for years from 2001 to 2010
  .(
    year,
    cnty_ttl_chg = sum(yoy_chg_ttl_acr, na.rm = TRUE),
    cnty_upl_chg = sum(yoy_chg_upl_acr, na.rm = TRUE),
    cnty_pma_chg = sum(yoy_chg_pma_acr, na.rm = TRUE),
    ttl_mills_chg = sum(yoy_chg_ttl_mills, na.rm = TRUE),
    up_mill_chg = sum(yoy_chg_up_mills, na.rm = TRUE),
    pma_mill_chg = sum(yoy_chg_pma_mills, na.rm = TRUE),
    ttl_exprt_chg = sum(yoy_chg_ttl_exprt, na.rm = TRUE),
    up_exprt_chg = sum(yoy_chg_up_exprt, na.rm = TRUE),
    pma_exprt_chg = sum(yoy_chg_pma_exprt, na.rm = TRUE)
  ),
  by = area_fips
][
  , # no row operations
  `:=`(
    pct_us_ttl_chg = cnty_ttl_chg / sum(cnty_ttl_chg, na.rm = TRUE),
    pct_us_upl_chg = cnty_upl_chg / sum(cnty_upl_chg, na.rm = TRUE),
    pct_us_pma_chg = cnty_pma_chg / sum(cnty_pma_chg, na.rm = TRUE),
    ttl_chg_wrt_mill = cnty_ttl_chg / ttl_mills_chg,
    up_chg_wrt_mill = cnty_upl_chg / up_mill_chg,
    pma_chg_wrt_mill_chg_10 = cnty_pma_chg / pma_mill_chg, # target variable relative change in acres contributed to us scaled by change in number of mills
    ttl_chg_wrt_exprt = cnty_ttl_chg / ttl_exprt_chg,
    up_chg_wrt_exprt = cnty_upl_chg / ttl_exprt_chg,
    pma_chg_wrt_exprt = cnty_pma_chg / ttl_exprt_chg,
    up_chg_wrt_up_exprt = cnty_upl_chg / up_exprt_chg,
    pma_chg_wrt_pma_exprt = cnty_pma_chg / pma_exprt_chg
  )
][
  pct_us_pma_chg == 0,
  `:=`(
    pct_us_pma_chg = NA,
    pma_chg_wrt_mills = NA,
    pma_chg_wrt_pma_exprt = NA
  )
]

map_data <- left_join(
  map_ntl_share_2000,
  map_ntl_share_2010,
  by = c("area_fips", "year")
)




########################################################################
# - 3 maps
########################################################################


#  join shape files to total dataset
map_full <- left_join(
  county_shapes,
  map_data,
  by = "area_fips"
)

map_full_test <- map_full[
  , # no row operations
  .(area_fips,
    pma_chg_wrt_mill_chg_00,
    pma_chg_wrt_mill_chg_10,
    )
]

tx_ca_pma_comp <- tm_shape(map_full) +
  tm_polygons(
    fill = c("pma_chg_wrt_mill_chg_00", "pma_chg_wrt_mill_chg_10"),
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
        style = "jenks",
        n = 5
      ),
    fill.legend = tm_legend(
        title = "Change in Area of ELS Cotton Scaled by The Change in ELS Cotton Mills"
      )
  ) +
      tm_layout(
        legend.outside = TRUE,
        frame = FALSE,
        bg.color = "transparent",
        outer.bg.color = "transparent"
      ) +
      tm_borders(lwd = 0)

  
  
  
  
  
  
  
  
  
  

# all changes total time periods
chg_area_up <- tm_shape(map) +
  tm_polygons(
    fill = "pct_us_upl_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Area of Upland Cotton As A Share of Total US Cotton 2010--2025"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

# all changes total time perios
chg_area_pma <- tm_shape(map) +
  tm_polygons(
    fill = "pct_us_pma_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Area of Pima Cotton As A Share of Total US Cotton"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_gr,
  filename = file.path(fig,".png"),
  width = 7,
  height = 5,
  dpi = 300
)


print(chg_area_up)

print(chg_area_pma)





# map of total change after great recession
tot_gr <- tm_shape(map_gr) +
  tm_polygons(
    fill = "pct_us_ttl_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Area of Planted Cotton As A Share of Total US Cotton 2010--2025"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_gr,
  filename = file.path(fig,"area_chg_2010_2025.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_gr)

# map of total change after china
tot_chin <- tm_shape(map_china) +
  tm_polygons(
    fill = "pct_us_ttl_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Area of Planted Cotton As A Share of Total US Cotton After China Shock"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_chin,
  filename = file.path(fig,"area_chg_china_shock.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_chin)

# change in area
total_nafta <- tm_shape(map_nafta) +
  tm_polygons(
    fill = "pct_us_ttl_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Area of Planted Cotton As A Share of Total US Cotton After NAFTA"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  total_nafta,
  filename = file.path(fig,"area_chg_nafta_shock.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(total_nafta)


## MILL CHANGE ##

# change in area wrt mill great recession
tot_mill_gr <- tm_shape(map_gr) +
  tm_polygons(
    fill = "ttl_chg_wrt_mill",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Planted Cotton Acres For Every Closed Cotton Mill After Great Recession"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_mill_gr,
  filename = file.path(fig, "mill_chg_ttl_gr.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_mill_gr)

# change in area wrt mill china shock
tot_mill_chin <- tm_shape(map_china) +
  tm_polygons(
    fill = "ttl_chg_wrt_mill",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Planted Cotton Acres For Every Closed Cotton Mill After China Shock"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_mill_gr,
  filename = file.path(fig, "mill_chg_ttl_chin.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_mill_chin)

# print map
print(tot_mill_chin)

# change in area wrt mill
tot_mill_nafta <- tm_shape(map_nafta) +
  tm_polygons(
    fill = "ttl_chg_wrt_mill",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Planted Cotton Acres For Every Closed Cotton Mill After NAFTA Shock"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_mill_gr,
  filename = file.path(fig, "mill_chg_ttl_nafta.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_mill_nafta)

# change in total area with respect to exports
tot_exprt_gr <- tm_shape(map_gr) +
  tm_polygons(
    fill = "ttl_chg_wrt_exprt",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Planted Cotton Acres With Respect to Change in Exports After Great Recession"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_exprt_gr,
  filename = file.path(fig, "exprt_chg_ttl_gr.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_exprt_gr)

# change in area wrt exprt china shock
tot_exprt_chin <- tm_shape(map_china) +
  tm_polygons(
    fill = "ttl_chg_wrt_exprt",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Planted Cotton Acres With Respect to Change in Exports After China Shock"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_exprt_chin,
  filename = file.path(fig, "exprt_chg_ttl_chin.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_exprt_chin)

# print map
print(tot_exprt_chin)

# change in area wrt exprt
tot_exprt_nafta <- tm_shape(map_nafta) +
  tm_polygons(
    fill = "ttl_chg_wrt_exprt",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "Change in Planted Cotton Acres With Respect to Change in Exports After NAFTA Shock"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

tmap_save(
  tot_exprt_gr,
  filename = file.path(fig, "exprt_chg_ttl_nafta.png"),
  width = 7,
  height = 5,
  dpi = 300
)

print(tot_exprt_nafta)

# change in area upland
up_china <- tm_shape(map_china) +
  tm_polygons(
    fill = "pct_us_upl_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "County Map of Change in Upland Acres Planted After Nafta"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent",
    outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

print(up_china)

# change in area planted pma 
pma_china <- tm_shape(map_china) +
  tm_polygons(
    fill = "pct_us_pma_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 5,
      value.na = "black"
    ),
    fill.legend = tm_legend(
      title = "County Map of Change in Pima Acres Planted After Nafta"
    )
  ) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "transparent", outer.bg.color = "transparent"
  ) +
  tm_borders(lwd = 0)

print(pma_china)













