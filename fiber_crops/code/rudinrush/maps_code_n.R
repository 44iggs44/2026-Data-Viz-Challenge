# Project: 2026 data viz challenge
# Created on: 13 July 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 15 Jul 2026
# R version 4.5.2

# note:
    # maps
    # llm was used for code assistance like typos and debugging. 
    # I used it in this case to translate a map from
    #   tmap to ggplot and matching aesthetics.
    #   original code for the tmap map can be seen on lines
    #   1066-1090 of code_graveyard.R in the rudinrush>old_code folder


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
# data <- file.path()
# 
# # figures folder
# fig <- file.path(
#   data, "figures"
# ) 


########################################################################
# - 1 county shape files
########################################################################

# get county shapes
us_counties <- counties(cb = TRUE, year = 2024, class = "sf") #|> st_transform(5070)

# state shapes for outlines
us_state <- states(cb = TRUE, year= 2024, class = "sf") #|> st_transform(5070) skews state to be accurate but not as aesthetic

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
    refined, "cotton_prod_use.csv"
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
    names(ca_tx_data_2000)[ # first set is the names of the variables that are numeric
        sapply(ca_tx_data_2000, is.numeric)
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

# initiates ggplot object for california
ca_map_gg <- ggplot(data = ca_data) + # selects california data
    
    # county line width
    geom_sf(aes(fill = indx_pima), color = "grey35", linewidth = 0.15)  +
    
    # color blind and black white scale safe color spectrum
    scale_fill_viridis_c(
        name = NULL, # Handled in the panel title/subtitle
        labels = label_number(accuracy = 0.1),
        guide = guide_colorbar(
            direction = "horizontal",
            barwidth = 18,
            barheight = 0.4,
            title.position = "top"
        )
    ) +
    labs(
        title = "California Pima Cotton Production Sensitivity between 2000 and 2020",
        subtitle = "Change in pima cotton production by county scaled by change in domestic pima demand."
    ) +
    theme_minimal(base_size = 11) +
    theme(
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9.5, color = "grey35"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), # Keeps map canvas perfectly clean
        legend.position = "bottom",
        legend.text = element_text(size = 8.5)
    )


# initiates ggplot object for texas data
tx_map_gg <- ggplot(data = tx_data) +
    geom_sf(aes(fill = indx_uplnd), color = "grey35", linewidth = 0.15) +
    scale_fill_viridis_c(
        name = NULL,
        labels = label_number(accuracy = 0.1),
        guide = guide_colorbar(
            direction = "horizontal",
            barwidth = 18,
            barheight = 0.4,
            title.position = "top"
        )
    ) +
    labs(
        title = "Texas Upland Production Sensitivity between 2000 and 2020",
        subtitle = "Change in upland cotton production scaled by the change in domestic upland cotton demand."
    ) +
    theme_minimal(base_size = 11) +
    theme(
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9.5, color = "grey35"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8.5)
    )

figure_title <- ggdraw() +
    draw_label(
        "Regional Cotton Production Sensitivity Analysis Across States",
        x = 0.01, y = 0.72, hjust = 0, fontface = "bold", size = 16 # title coords and size
    ) +
    draw_label(
        "Comparison of Production Sensitivity for California Pima Cotton and Texas Upland S.",
        x = 0.01, y = 0.25, hjust = 0, size = 10.5, color = "grey35"
    )

figure_caption <- ggdraw() +
    draw_label(
        paste0(
            "Note: Values represent a sensitivity index cotton production to changes in domestic mill use.\n", 
            "The numerator is the change in each county's share of domestic cotton production for pima in California and upland in Texas in thousands of bales between the years 2020 and 2000.\n", 
            "The denominator is the change in mill usage in thousands of bales of pima cotton for the California index values and upland cotton for the Texas index values between the years 2020 and 2000.\n",
            "Sources: USDA Economic Research Service (ERS) Cotton and Wool Outlook Reports.\n",
            "AI Disclaimer: Lorin used generative AI to translate tmap code to ggplot2 code."
        ),
        x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5, size = 8, lineheight = 1.2, color = "black"
    )

spatial_comparison_combined <- plot_grid(
    figure_title,
    plot_grid(
        ca_map_gg,
        tx_map_gg,
        nrow = 1,
        rel_widths = c(1, 1),
        align = "h",
        axis = "tb"
    ),
    figure_caption,
    ncol = 1,
    rel_heights = c(0.15, 1, 0.1) # Allocates clean visual margins
)

# Print final map layout
print(spatial_comparison_combined)

# save object
ggsave(
    filename = file.path(
        fig,
        "ca_tx_prod_to_usage.png"
    ),
    plot = spatial_comparison_combined,
    width = 15,
    height = 8.5,
    dpi = 300,
    bg = "white"
)



## END ##

