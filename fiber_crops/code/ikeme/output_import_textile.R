

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)

# ==============================================================================
# Output and imports in US DOLLARS for textile industries (NAICS 313, 314, 315)
#

# include AIS keys
census_key <- "32514956a9d5e79bc7d1bb3437726ff7627c7d22"   # https://api.census.gov/data/key_signup.html
fred_key   <- "2e50d059cd97db9577407e4a1a076653"     # https://fred.stlouisfed.org/docs/api/api_key.html
# ==============================================================================
# data obtained from 
#
#   1) output  1990-2018  -> NBER-CES Manufacturing Database (value of shipments, nominal $). 
#   2)  imports 1997-2025  -> USITC DataWeb (general imports, nominal $)..
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)

setwd("/data/sikeme/data_visualization")

naics_label <- c("313" = "Textile mills",
                 "314" = "Textile product mills",
                 "315" = "Apparel manufacturing")

# ----------------------------------------------------------------------------
# 1): OUTPUT in dollars from NBER-CES
# ----------------------------------------------------------------------------

nberces <-  read_csv("data_visualization/data/nberces5818v1_n2012.csv")

output <- nberces %>%
  mutate(naics3 = str_sub(as.character(naics), 1, 3)) %>%
  filter(naics3 %in% names(naics_label)) %>%
  group_by(year, naics3) %>%
  summarise(
    vship_musd = sum(vship, na.rm = TRUE),                 # nominal $ millions
    # real $ (1997 dollars): divide nominal shipments by the shipment deflator
    vship_real_musd = sum(vship / piship, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    year,
    naics = naics3,
    industry = naics_label[naics3],
    output_nominal_usd = vship_musd * 1e6,                 # convert to dollars
    output_real1997_usd = vship_real_musd * 1e6
  ) %>%
  filter(year >= 1990)

write_csv(output, "textile_output_dollars_1990_2018.csv")

# ----------------------------------------------------------------------------
# 2) IMPORTS in dollars from USITC DataWeb
# ----------------------------------------------------------------------------

library(readxl)
imports_raw <- read_excel("data_visualization/data/dataweb_textile_imports.xlsx", 
                                      sheet = "General Customs Value")
names(imports_raw)  # <- your exported file
imports_raw <- imports_raw %>% filter(`NAIC Number` %in% c(313, 314, 315))


# Rename to match: adjust these to the actual column headers in your export.
imports <- imports_raw %>%
  rename(year = Year, naics = `NAIC Number`, imports_usd = `General Customs Value`) %>%
  mutate(naics = as.character(naics),
         industry = naics_label[naics],
         imports_usd = as.numeric(imports_usd))%>%
  select(year, naics, industry, imports_usd)

# ----------------------------------------------------------------------------
# COMBINE into one tidy long table (common NAICS-consistent window: 1997+)
# ----------------------------------------------------------------------------

combined <- bind_rows(
  output  %>% transmute(year, naics, industry,
                        series = "output_nominal_usd", value = output_nominal_usd),
  imports %>% transmute(year, naics, industry,
                        series = "imports_usd", value = imports_usd)
) %>%
  mutate(industry = paste0(naics, " - ", naics_label[naics])) %>%
  arrange(naics, series, year)

table(combined$naics,combined$industry)
combined <- combined %>% recode()

write_csv(combined, "data_visualization/data/textile_output_imports_dollars.csv")
print(head(combined)); print(tail(combined))

###############################################################################
# get plot for visualization:

library(ggplot2)
library(scales)
plot_data <- combined %>%
  # keep only the years both series NEED to overlap; imports floor at 1997
  # filter(year >= 1997) %>%
  mutate(series_label = recode(series,
                               output_nominal_usd = "Domestic output (value of shipments)",
                               imports_usd = "Imports (customs value)"))

p <- ggplot(plot_data, aes(x = year, y = value, color = series_label)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ industry, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("Domestic output (value of shipments)" = "#2a78d6",
                                "Imports (customs value)" = "#eb6834")) +
  labs(
    x = NULL, y = "US dollars (nominal)", color = NULL,
    title = "Domestic textile output vs. imports, 1990-2025"  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))
p
ggsave("data_visualization/plots/textile_output_vs_imports.png", p, width = 8, height = 10, dpi = 300)
print(p)






