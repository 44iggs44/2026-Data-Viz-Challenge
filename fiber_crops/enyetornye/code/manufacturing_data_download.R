

library(here)

# Local directory for downloaded annual BLS QCEW files
download_dir <- here(
  "fiber_crops",
  "enyetornye",
  "data",
  "raw"
)


# Download and extract all annual by-industry files
for (year in 1990:2025) {
  
  year_folder <- file.path(
    download_dir,
    paste0(
      year,
      ".annual.by_industry"
    )
  )
  
  existing_csvs <- list.files(
    year_folder,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Skip years already downloaded
  if (length(existing_csvs) > 0) {
    message("Already exists, skipped: ", year)
    next
  }
  
  url <- sprintf(
    "https://data.bls.gov/cew/data/files/%d/csv/%d_annual_by_industry.zip",
    year,
    year
  )
  
  zip_file <- tempfile(
    pattern = paste0(year, "_"),
    fileext = ".zip"
  )
  
  extract_folder <- tempfile(
    pattern = paste0(year, "_extract_")
  )
  
  dir.create(
    year_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    extract_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  download.file(
    url = url,
    destfile = zip_file,
    mode = "wb"
  )
  
  unzip(
    zipfile = zip_file,
    exdir = extract_folder
  )
  
  csv_files <- list.files(
    extract_folder,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  file.copy(
    from = csv_files,
    to = year_folder,
    overwrite = FALSE
  )
  
  unlink(
    c(
      zip_file,
      extract_folder
    ),
    recursive = TRUE
  )
  
  message("Completed: ", year)
}

