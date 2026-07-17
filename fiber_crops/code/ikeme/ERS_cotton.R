# library(readr)
# library(ggplot2)
# library(patchwork)
# 
# 
# library(readr)
# library(tidyr)
# library(dplyr)
# library(data.table)
# library(stringi)
# library(fixest)
# library(countrycode)
# library(tidyverse)
# library(vroom)
# 
# 


base <- "https://www.ers.usda.gov/media/"

urls <- c(
  us_supply_demand    = paste0(base, "7122/us-cotton-supply-and-demand.csv"),
  cotton_prices       = paste0(base, "7123/cotton-prices.csv"),
  world_supply_demand = paste0(base, "7124/world-cotton-supply-and-demand.csv"),
  us_fiber_demand     = paste0(base, "7127/us-fiber-demand.csv"),
  us_textile_trade    = paste0(base, "7125/us-textile-fiber-trade.csv")
)

# Read one table
us_cotton <- read_csv(urls["us_supply_demand"])

cotton_data <- lapply(urls, read_csv)



library(data.table)

# Convert each element in place
cotton_dt <- lapply(cotton_data, as.data.table)

# Access each data
supply <- cotton_dt$us_supply_demand
prices <- cotton_dt$cotton_prices
textile_trade <- cotton_dt$us_textile_trade

# write supply file to folder
fwrite(
    supply,
    file = file.path(cotton, "ers", "us-cotton-supply-and-demand.csv")
)


# #################################################################################
# # get cotton prices over time at yearly level
# #################################################################################
# 
# names(prices)
# unique(prices$table_name)
# unique(prices$category)
# 
# prices1 <- prices %>% filter(table_name == "U.S. upland cotton farm, spot, and mill prices, 1975/76–2024/25")
# unique(prices1$period)
# unique(prices1$time_period)
# unique(prices1$category)
# 
# prices1 <- prices1 %>% select(period, category,value, units)
# prices1$value <- as.numeric(prices1$value)
# 
# prices1 <- prices1 %>% filter(period %in% 1980:2022)
# 
# # Price line chart
# price_plot <- ggplot(prices1, aes(x = period, y = value, color = category)) +
#   geom_line(linewidth = 0.8) +
#   labs(x = NULL, y = "Price (Cents per pound)", color = NULL,
#        title = "U.S. Cotton Prices") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank())
# price_plot
# 
# 
# #################################################################################
# # Cotton Supply and Demand 
# #################################################################################
# 
# 
# names(supply)
# unique(supply$table_name)
# 
# supply_use <- supply %>% filter( table_name == "U.S. cotton supply and use, 1975/76–2025/26")
# unique(supply_use$period)
# unique(supply_use$period_name)
# unique(supply_use$Type)
# unique(supply_use$category)
# unique(supply_use$unit)
# 
# 
# plot_data <- supply_use %>%
#   filter(category %in% c("production", "imports", "mill_use", "qty_of_exports"),
#          period %in% 1980:2022) %>%
#   mutate(
#     side = if_else(category %in% c("production", "imports"), "Supply", "Demand"),
#     category = factor(category,
#                       levels = c("production", "imports", "mill_use", "qty_of_exports"),
#                       labels = c("Production", "Imports", "Mill Use", "Exports"))
#   )
# 
# supply_demand_plot <- ggplot(plot_data, aes(x = factor(period), y = value, fill = category)) +
#   geom_col(position = "stack") +
#   facet_wrap(~ side, ncol = 1) +
#   labs(x = "Year", y = "Quantity (1,000 480-lb bales)", fill = NULL,
#        title = "U.S. Cotton Supply and Demand") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# supply_demand_plot
# 
# 
# 
# 
# price_plot / supply_demand_plot +
#   plot_layout(heights = c(1, 2))
