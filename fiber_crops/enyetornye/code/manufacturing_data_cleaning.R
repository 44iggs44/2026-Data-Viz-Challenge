

# Load packages
library(here)
library(readr)
library(stringr)
library(data.table)

# Directory containing the annual QCEW files created by
# manufacturing_data_download.R
raw_dir <- here(
  "fiber_crops",
  "enyetornye",
  "data",
  "raw"
)

# Directory for the harmonized manufacturing dataset
data_dir <- here(
  "fiber_crops",
  "enyetornye",
  "data"
)

# Textile- and fiber-related NAICS industries
target_naics <- c(
  "3131",
  "3132",
  "3133",
  "3141",
  "3149",
  "3152",
  "3159",
  "3252"
)

# Identify all CSV files in the downloaded annual-data folders
all_files <- list.files(
  path = raw_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Retain annual files corresponding to the selected NAICS industries
fiber_files <- all_files[
  str_detect(
    basename(all_files),
    paste0(
      "^\\d{4}\\.annual (",
      paste(
        target_naics,
        collapse = "|"
      ),
      ")\\d* "
    )
  )
]

# Display the number of files selected for harmonization
length(fiber_files)

# Import and combine the selected annual files for 1990–2025
data_1990_2025 <- rbindlist(
  lapply(
    fiber_files,
    function(x) {
      dt <- fread(
        file = x
      )
      
      # Preserve the original filename for verification
      dt[
        ,
        source_file := basename(x)
      ]
      
      dt
    }
  ),
  fill = TRUE
)

# Save the harmonized manufacturing dataset
write_csv(
  data_1990_2025,
  file.path(
    data_dir,
    "fiber_manufacturing_all.csv"
  )
)