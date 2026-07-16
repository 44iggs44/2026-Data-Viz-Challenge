

library(here)
library(tidyverse)
library(readr)
library(stringr)
library(data.table)

path <- here( "master_data")

target_naics <- c("3131", "3132", "3133", "3141", "3149", "3152", "3159", "3252")

all_files <- list.files(
  path = path,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

fiber_files <- all_files[
  str_detect(
    basename(all_files),
    paste0("^\\d{4}\\.annual (", paste(target_naics, collapse = "|"), ")\\d* ")
  )
]

length(fiber_files)


data_1990_2025 <- rbindlist(
  lapply(fiber_files, function(x) {
    dt <- fread(x)
    dt[, source_file := basename(x)]
    dt
  }),
  fill = TRUE
)


write_csv(
  data_1990_2025,
  file.path(path, "fiber_manufacturing_all.csv")
)
