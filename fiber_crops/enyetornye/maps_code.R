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
cali_shape <- us_counties[
  us_counties$STATEFP == "06", # selects california and texas for comparison, 
] |> st_transform(5070)

# download county data select california and texas for comparison
tx_shape <- us_counties[
  us_counties$STATEFP == "48", # selects california and texas for comparison, 
] |> st_transform(5070)



# change fips to match cotton and manufacturing data
cali_shape <- cali_shape |>
  rename(area_fips = GEOID)

tx_shape <- tx_shape |>
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

# create dataset of values from 2001-2010 for map specifics
map_ntl_share_2010 <- cttn_pdctn[
  year %in% 2001:2010 &
  state_alpha %in% c("CA", "TX"), # selects for years from 2001 to 2010
  .(
    year,
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
    pma_chg_wrt_mill_chg_10 = cnty_pma_chg / pma_mill_chg 
  )
]


mill_chg_10 <- map_ntl_share_2010[
  , # no row operations
  .(
    area_fips,
    pma_chg_wrt_mill_chg_10
  )
] |> unique()

mill_chg_00 <- map_ntl_share_2000[
  , # no row ops
  .(
    area_fips,
    pma_chg_wrt_mill_chg_00
  )
] |> unique()


map_data <- left_join(
  mill_chg_00,
  mill_chg_10,
  by = c("area_fips")
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
  c(area_fips,
    pma_chg_wrt_mill_chg_00,
    pma_chg_wrt_mill_chg_10
  )
]



# map check 
plot_state_period_comparison_2(
  data = cttn_pdctn,
  shapes = county_shapes,
  states = c("CA", "TX"),
  metric = "pma_chg_wrt_mill_chg",
  periods = list(1990:2000, 2001:2010),
  period_labels = c("1990-2000","2001-2010"),
  state_col = "state_alpha",
  year_col = "year",
  fips_col = "area_fips"
)


# Texas and California, 2000 vs 2010
tx_ca_pma_comp <- rbind(
  ca_tx_data[
    year %in% 1990:2000 & state_alpha %in% c("TX", "CA"),
    .(
      state_alpha,
      end_year = "2000",
      pma_chg_wrt_mill_chg = sum(pma_chg_wrt_mill_chg, na.rm = TRUE)
    ),
    by = area_fips
  ],
  ca_tx_data[
    year %in% 2001:2010 & state_alpha %in% c("TX", "CA"),
    .(
      state_alpha,
      end_year = "2010",
      pma_chg_wrt_mill_chg = sum(pma_chg_wrt_mill_chg, na.rm = TRUE)
    ),
    by = area_fips
  ],
  fill = TRUE
)




tx_ca_pma_comp[
  ,
  year := NULL
]

texas_pma <- tx_ca_pma_comp[
  state_alpha == "TX"
]

tx_pma$period <- factor(
  tx_pma$period,
  levels = c("2000", "2010")
)

cali_pma <- tx_ca_pma_comp[
  state_alpha == "CA"
] |> unique()

ca_pma <- left_join(
  cali_shape,
  cali_pma,
  by = "area_fips"
)

tx_pma <- left_join(
  tx_shape,
  texas_pma,
  by = "area_fips"
)

ca_pma$period <- factor(
  ca_pma$period,
  levels = c("2000", "2010")
)

ca_pma_unq_00 <- ca_pma |>
  filter(!is.na(pma_chg_wrt_mill_chg))


tx_pma_unq <- tx_pma  |>
  filter(!is.na(pma_chg_wrt_mill_chg))


ca_map <- tm_shape(ca_pma_unq) +
  tm_polygons(
    fill = "pma_chg_wrt_mill_chg",
    fill.scale = tm_scale_intervals(
      values = "pu_gn",
      style = "jenks",
      n = 10
    ),
    fill.legend = tm_legend(
    title = "Jenks Scaled Quantiles"
    )
  ) +
  tm_facets_grid(columns = "period") +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE
  ) +
  tm_borders(lwd = 0.5)

tx_map <- tm_shape(tx_pma_unq) +
  tm_polygons(
    fill = "pma_chg_wrt_mill_chg",
    fill.scale = tm_scale_intervals(
      values = "pu_gn",
      style = "jenks",
      n = 10
    ),
    fill.legend = tm_legend(
      title = "Jenks Scaled Quantiles"
    )
  ) +
  tm_facets_grid(columns = "period") +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE
  ) +
  tm_borders(lwd = 0.5)

stacked_map <- tmap_arrange(
  tx_map,
  ca_map,
  ncol = 1,
  nrow = 2
)

print(stacked_map)


















tx_ca_pma_comp_unq <- unique(tx_ca_pma_comp)

tx_ca_pma_comp_unq[
  !is.na(state_alpha) & !is.na(period), # no row op
  .(
    area_fips,
    state_alpha,
    period,
    pma_chg_wrt_mill_chg
  )
]


na_subset <- copy(tx_ca_pma_comp_unq[is.na(tx_ca_pma_comp_unq$period)])



tx_ca_pma_comp_unq <- left_join(
  county_shapes,
  tx_ca_pma_comp_unq,
  by = "area_fips"
)



tx_ca_pma_comp_unq$STUSPS <- factor(tx_ca_pma_comp_unq$STUSPS, levels = c("TX", "CA"))
tx_ca_pma_comp_unq$period <- factor(tx_ca_pma_comp_unq$period, levels = c("2000", "2010"))








tx_ca_pma_map <- tm_shape(tx_ca_pma_comp_unq[!is.na(c("period", "state_alpha"))]) +
  tm_polygons(
    fill = "pma_chg_wrt_mill_chg",
    col_alpha = 0,
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 5
    ),
    fill.legend = tm_legend(
      title = "Change in Area of ELS Cotton Scaled by The Change in ELS Cotton Mills"
    )
  ) +
  tm_facets_grid(rows = "period", columns = "STUSPS") +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE
  ) +
  tm_borders(lwd = 1) +
  tm_options(legend.na.show = FALSE)

print(tx_ca_pma_map)

  

plot_state_period_comparison(
  
)


ggplot2::ggplot(plot_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = value), color = NA) +
  ggplot2::facet_grid(period_panel ~ state_panel) +
  ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey85") +
  ggplot2::labs(
    title = paste0("Side-by-side comparison of ", metric),
    subtitle = paste(period_labels, collapse = " vs "),
    fill = metric
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ca_tx_map <- ggplot(ca_tx_data)  +
  geom_sf(
    aes(
      fill = 
    )
  )












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
    col = ""
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













