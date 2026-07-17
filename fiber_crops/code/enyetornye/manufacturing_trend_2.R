


# ==============================================================================
# Visualization 2: Textile Trade and Regional Manufacturing Trends
#
# Purpose:
# This script compares national textile output, imports, and cotton-equivalent
# imports with regional textile employment and establishment trends.
#
# Main inputs:
# 1. cttn_mftr.csv
#    Cleaned BLS QCEW textile manufacturing data.
# 2. textile_output_imports_dollars.csv
#    National textile output and import values.
# 3. Table1_US_textile_imports_by_fiber.xlsx
#    USDA ERS textile imports measured by fiber weight.
# 4. cotton_harmonized.csv
#    Clean cotton-production data used for the report calculations.
#
# Output:
# trade_activity_combined.png
#
# All indexed series use 1997 as the reference year, where 1997 = 100.
# One statistical cotton bale is defined as 480 pounds.
# ==============================================================================


# 
# # Load packages
# library(data.table)
# library(dplyr)
# library(tidyr)
# library(here)
# library(stringr)
# library(readxl)
# library(ggplot2)
# library(cowplot)
# library(scales)

# Set the reference year and cotton-bale conversion factor
base_year <- 1997
cotton_bale_weight_lb <- 480

# Define the three cotton and textile regions
region_levels <- c(
    "Southwest/West",
    "Delta/Mid-South",
    "Southeast/Textile Belt"
)

# Assign a consistent color to each region
region_colors <- c(
    "Southwest/West" = "#D95F59",
    "Delta/Mid-South" = "#6A4C93",
    "Southeast/Textile Belt" = "#238B8E"
)



# Import QCEW manufacturing data

# Create a temporary directory for the extracted CSV
# this is to help with the comprised data that is > 100mb

# # Locate the compressed manufacturing dataset
# manufacturing_zip <- here(
#     "fiber_crops",
#     "enyetornye",
#     "data",
#     "cttn_mftr.csv.zip"
# )
# 
# # Create a temporary directory for the extracted CSV
# manufacturing_temp_dir <- file.path(
#     tempdir(),
#     "cttn_mftr"
# )
# 
# 
# # Extract the CSV from the ZIP file
# unzip(
#     manufacturing_zip,
#     exdir = manufacturing_temp_dir
# )

# Import the extracted manufacturing dataset
manufacturing <- fread(
    file = file.path(
        refined,
        "manufacturing",
        "cttn_mftr.csv"
    )
)

# Prepare state-level QCEW data
manufacturing_state <- manufacturing %>%
    mutate(
        year = as.integer(year),
        area_fips = str_pad(
            as.character(area_fips),
            width = 5,
            side = "left",
            pad = "0"
        ),
        state_fips = str_sub(
            area_fips,
            1,
            2
        ),
        industry_code = str_remove(
            as.character(industry_code),
            "\\.0$"
        ),
        annual_avg_emplvl = as.numeric(
            annual_avg_emplvl
        ),
        annual_avg_estabs_count = as.numeric(
            annual_avg_estabs_count
        ),
        region = case_when(
            state_fips %in% c(
                "04", "06", "20", "35", "40", "48"
            ) ~ "Southwest/West",
            
            state_fips %in% c(
                "05", "22", "28", "29", "47"
            ) ~ "Delta/Mid-South",
            
            state_fips %in% c(
                "01", "13", "37", "45", "51"
            ) ~ "Southeast/Textile Belt",
            
            TRUE ~ NA_character_
        )
    ) %>%
    filter(
        year %in% 1990:2025,
        qtr == "A",
        own_code == 5,
        size_code == 0,
        agglvl_code == 56,
        !is.na(region)
    )

# Use three-digit textile sectors when available
if (
    all(
        c("313", "314", "315") %in%
        unique(manufacturing_state$industry_code)
    )
) {
    
    manufacturing_textile <- manufacturing_state %>%
        filter(
            industry_code %in% c(
                "313",
                "314",
                "315"
            )
        )
    
} else {
    
    manufacturing_textile <- manufacturing_state %>%
        filter(
            str_detect(
                industry_code,
                "^(313|314|315)\\d$"
            )
        )
}

# Aggregate and index regional employment and establishments
regional_activity <- manufacturing_textile %>%
    group_by(
        year,
        region
    ) %>%
    summarise(
        employment = sum(
            annual_avg_emplvl,
            na.rm = TRUE
        ),
        establishments = sum(
            annual_avg_estabs_count,
            na.rm = TRUE
        ),
        .groups = "drop"
    ) %>%
    pivot_longer(
        cols = c(
            employment,
            establishments
        ),
        names_to = "measure",
        values_to = "value"
    ) %>%
    group_by(
        region,
        measure
    ) %>%
    mutate(
        base_value = first(
            value[year == base_year]
        ),
        value_index = 100 * value / base_value
    ) %>%
    ungroup() %>%
    mutate(
        region = factor(
            region,
            levels = region_levels
        ),
        measure = recode(
            measure,
            "employment" = "Employment",
            "establishments" = "Establishments"
        ),
        measure = factor(
            measure,
            levels = c(
                "Employment",
                "Establishments"
            )
        )
    )


# Import textile output and import values
data_dir <- here(
    "fiber_crops",
    "enyetornye",
    "data"
)

# Import textile output and import values
output_import <- fread(
    file = file.path(
        data_dir,
        "textile_output_imports_dollars.csv"
    )
) %>%
    as_tibble() %>%
    mutate(
        year = as.integer(year),
        naics = as.character(naics),
        value = as.numeric(value)
    ) %>%
    filter(
        naics %in% c(
            "313",
            "314",
            "315"
        ),
        series %in% c(
            "output_nominal_usd",
            "imports_usd"
        )
    ) %>%
    group_by(
        year,
        series
    ) %>%
    summarise(
        value = sum(
            value,
            na.rm = TRUE
        ),
        .groups = "drop"
    ) %>%
    group_by(series) %>%
    mutate(
        base_value = first(
            value[year == base_year]
        ),
        value_index = 100 * value / base_value
    ) %>%
    ungroup() %>%
    mutate(
        series_label = recode(
            series,
            "output_nominal_usd" = "Domestic output value",
            "imports_usd" = "Import value"
        )
    )

# Import USDA ERS cotton raw-fiber-equivalent data
fiber_raw <- read_excel(
    file.path(
        data_dir,
        "Table1_US_textile_imports_by_fiber.xlsx"
    ),
    sheet = "table 1",
    col_names = FALSE
)

# Identify valid year columns
fiber_year_values <- unlist(
    fiber_raw[
        2,
        3:ncol(fiber_raw)
    ],
    use.names = FALSE
)

valid_years <- !is.na(
    suppressWarnings(
        as.integer(fiber_year_values)
    )
)

fiber_years <- as.integer(
    fiber_year_values[valid_years]
)

fiber_year_columns <- which(valid_years) + 2

# Exclude spreadsheet notes appearing below the final data row
note_row <- which(
    str_detect(
        as.character(
            fiber_raw[[1]]
        ),
        "^Note:"
    )
)[1]

fiber_end <- if (
    is.na(note_row)
) {
    nrow(fiber_raw)
} else {
    note_row - 1
}

# Reshape cotton-equivalent import data
fiber_cotton_categories <- fiber_raw[
    4:fiber_end,
    c(
        1,
        2,
        fiber_year_columns
    )
] %>%
    setNames(
        c(
            "item",
            "fiber",
            as.character(fiber_years)
        )
    ) %>%
    mutate(
        across(
            matches("^\\d{4}$"),
            ~ suppressWarnings(
                as.numeric(.x)
            )
        )
    ) %>%
    fill(item) %>%
    filter(
        str_to_lower(
            str_trim(fiber)
        ) == "cotton",
        str_to_lower(
            str_trim(item)
        ) != "all textiles"
    ) %>%
    pivot_longer(
        cols = matches("^\\d{4}$"),
        names_to = "year",
        values_to = "cotton_equivalent_lb"
    ) %>%
    mutate(
        year = as.integer(year)
    )

# Convert cotton-equivalent imports into million bales
fiber_equivalent <- fiber_cotton_categories %>%
    group_by(year) %>%
    summarise(
        cotton_equivalent_lb = sum(
            cotton_equivalent_lb,
            na.rm = TRUE
        ),
        .groups = "drop"
    ) %>%
    mutate(
        cotton_equivalent_million_bales =
            cotton_equivalent_lb /
            cotton_bale_weight_lb /
            1e6
    )

# Scale cotton-equivalent bales to the primary plotting axis
fiber_scale <- max(
    output_import$value_index,
    na.rm = TRUE
) / max(
    fiber_equivalent$
        cotton_equivalent_million_bales,
    na.rm = TRUE
)

# Left panel
trade_fiber_plot <- ggplot() +
    geom_hline(
        yintercept = 100,
        linetype = "dashed",
        color = "grey55",
        linewidth = 0.4
    ) +
    geom_line(
        data = output_import,
        aes(
            x = year,
            y = value_index,
            color = series_label
        ),
        linewidth = 1.15
    ) +
    geom_line(
        data = fiber_equivalent,
        aes(
            x = year,
            y = cotton_equivalent_million_bales *
                fiber_scale,
            color = "Cotton-equivalent imports"
        ),
        linewidth = 1.15,
        linetype = "22"
    ) +
    scale_color_manual(
        values = c(
            "Domestic output value" = "#238B8E",
            "Import value" = "#D95F59",
            "Cotton-equivalent imports" = "#6A4C93"
        ),
        breaks = c(
            "Domestic output value",
            "Import value",
            "Cotton-equivalent imports"
        ),
        name = NULL
    ) +
    scale_x_continuous(
        limits = c(
            1989,
            2025
        ),
        breaks = c(
            1990,
            1995,
            2000,
            2005,
            2010,
            2015,
            2020,
            2025
        )
    ) +
    scale_y_continuous(
        name = "Nominal-dollar index (1997 = 100)",
        labels = label_number(
            accuracy = 1
        ),
        sec.axis = sec_axis(
            ~ . / fiber_scale,
            name = paste(
                "Cotton raw-fiber equivalent",
                "(million bales)"
            ),
            labels = label_number(
                accuracy = 1
            )
        )
    ) +
    labs(
        title = "Textile Output Value, Import Value, and Cotton Equivalents",
        subtitle = paste(
            "Output value: 1990–2018; import value: 1997–2025;",
            "cotton-equivalent imports: 1989–2025."
        ),
        x = NULL
    ) +
    guides(
        color = guide_legend(
            nrow = 2,
            byrow = TRUE
        )
    ) +
    theme_minimal(
        base_size = 11
    ) +
    theme(
        plot.title = element_text(
            size = 14,
            face = "bold"
        ),
        plot.subtitle = element_text(
            size = 9.5,
            color = "grey35"
        ),
        axis.title.y = element_text(
            size = 10,
            face = "bold"
        ),
        axis.title.y.right = element_text(
            size = 10,
            face = "bold"
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(
            size = 8.5
        )
    )

# Right panel
regional_activity_plot <- ggplot(
    regional_activity,
    aes(
        x = year,
        y = value_index,
        color = region
    )
) +
    geom_hline(
        yintercept = 100,
        linetype = "dashed",
        color = "grey55",
        linewidth = 0.4
    ) +
    geom_line(
        linewidth = 1.05
    ) +
    facet_wrap(
        ~measure,
        ncol = 1
    ) +
    scale_color_manual(
        values = region_colors,
        name = NULL
    ) +
    scale_x_continuous(
        limits = c(
            1990,
            2025
        ),
        breaks = c(
            1990,
            1995,
            2000,
            2005,
            2010,
            2015,
            2020,
            2025
        )
    ) +
    scale_y_continuous(
        labels = label_number(
            accuracy = 1
        )
    ) +
    labs(
        title = "Regional Textile Employment and Establishments",
        subtitle = paste(
            "Aggregated NAICS 313–315;",
            "each regional series indexed to 1997 = 100."
        ),
        x = NULL,
        y = "Index (1997 = 100)"
    ) +
    guides(
        color = guide_legend(
            nrow = 1,
            byrow = TRUE
        )
    ) +
    theme_minimal(
        base_size = 11
    ) +
    theme(
        plot.title = element_text(
            size = 14,
            face = "bold"
        ),
        plot.subtitle = element_text(
            size = 9.5,
            color = "grey35"
        ),
        axis.title.y = element_text(
            size = 10,
            face = "bold"
        ),
        strip.text = element_text(
            size = 10.5,
            face = "bold"
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(
            size = 8.5
        )
    )

# Overall title
figure_title <- ggdraw() +
    draw_label(
        "Textile Import Values Expanded While Domestic Manufacturing Contracted",
        x = 0.01,
        y = 0.72,
        hjust = 0,
        fontface = "bold",
        size = 18
    ) +
    draw_label(
        paste(
            "National textile-market indicators and regional",
            "manufacturing employment and establishments, 1989–2025."
        ),
        x = 0.01,
        y = 0.25,
        hjust = 0,
        size = 10.5,
        color = "grey35"
    )


# Calculate 1997 reference values
output_base_value <- output_import %>%
    filter(
        year == base_year,
        series == "output_nominal_usd"
    ) %>%
    pull(value)

import_base_value <- output_import %>%
    filter(
        year == base_year,
        series == "imports_usd"
    ) %>%
    pull(value)

cotton_base_lb <- fiber_equivalent %>%
    filter(year == base_year) %>%
    pull(cotton_equivalent_lb)

cotton_base_million_bales <- fiber_equivalent %>%
    filter(year == base_year) %>%
    pull(cotton_equivalent_million_bales)


# Calculate 1997 regional reference values
regional_base_values <- regional_activity %>%
    filter(year == base_year) %>%
    select(
        region,
        measure,
        value
    ) %>%
    pivot_wider(
        names_from = measure,
        values_from = value
    ) %>%
    arrange(region)

# Format 1997 regional reference values
regional_reference_note <- regional_base_values %>%
    mutate(
        reference_text = paste0(
            region,
            " = ",
            scales::comma(
                Employment,
                accuracy = 1
            ),
            " employees and ",
            scales::comma(
                Establishments,
                accuracy = 1
            ),
            " establishments"
        )
    ) %>%
    pull(reference_text) %>%
    paste(
        collapse = "; "
    )

# Source and measurement note
figure_caption <- ggdraw() +
    draw_label(
        paste0(
            "Note: In the left panel, an index of 100 represents the 1997 domestic output value of ",
            scales::dollar(
                output_base_value,
                scale = 1e-9,
                suffix = " billion",
                accuracy = 0.1
            ),
            " and import value of ",
            scales::dollar(
                import_base_value,
                scale = 1e-9,
                suffix = " billion",
                accuracy = 0.1
            ),
            ". Cotton-equivalent imports are actual values; in 1997, they equaled ",
            scales::number(
                cotton_base_million_bales,
                accuracy = 0.1
            ),
            " million bales (",
            scales::number(
                cotton_base_lb / 1e9,
                accuracy = 0.1
            ),
            " billion pounds).\n",
            "Right-panel index of 100 represents each region's 1997 level: ",
            regional_reference_note,
            ".\n",
            "Sources: Textile output and import values; USDA ERS; and BLS Quarterly Census of Employment and Wages."
        ),
        x = 0.5,
        y = 0.5,
        hjust = 0.5,
        vjust = 0.5,
        size = 7.9,
        lineheight = 1.2,
        color = "black"
    )

# Combine panels
trade_activity_combined <- plot_grid(
    figure_title,
    plot_grid(
        trade_fiber_plot,
        regional_activity_plot,
        nrow = 1,
        rel_widths = c(
            1.15,
            1
        ),
        align = "h",
        axis = "tb"
    ),
    figure_caption,
    ncol = 1,
    rel_heights = c(
        0.12,
        1,
        0.12
    )
)

trade_activity_combined

#figures dir
figures_dir <- here(
    "fiber_crops",
    "enyetornye",
    "figures"
)

#save figures
ggsave(
    filename = file.path(
        fig,
        "trade_activity_combined.png"
    ),
    plot = trade_activity_combined,
    width = 15,
    height = 8.5,
    dpi = 300,
    bg = "white"
)



# for report write-up

cotton_import_2025 <- fiber_equivalent %>%
    filter(year == 2025) %>%
    pull(cotton_equivalent_million_bales)

cotton_import_2025

# Import the cleaned cotton production dataset
cotton <- fread(
    file = file.path(
        refined,
        "cotton",
        "cotton_harmonized.csv"
    )
)

# Cotton-equivalent imports in 2025
cotton_import_2025 <- fiber_equivalent %>%
    filter(year == 2025) %>%
    pull(cotton_equivalent_million_bales)

# Total U.S. cotton production in 2025
total_us_cotton_production_million_bales <- cotton %>%
    filter(
        year == 2025,
        county_code != 998
    ) %>%
    mutate(
        cotton_bales = rowSums(
            across(
                c(
                    upland_bales,
                    pima_bales
                )
            ),
            na.rm = TRUE
        )
    ) %>%
    summarise(
        total_bales = sum(
            cotton_bales,
            na.rm = TRUE
        ) / 1e6
    ) %>%
    pull(total_bales)

# Import share
cotton_import_share <- 100 *
    cotton_import_2025 /
    total_us_cotton_production_million_bales

cotton_import_2025
total_us_cotton_production_million_bales
cotton_import_share



