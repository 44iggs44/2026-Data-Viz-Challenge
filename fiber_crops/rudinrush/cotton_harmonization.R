# Project: 2026 data viz challenge 
# Created on: 01 Jul 2026
# Created by: lirr
# Edited by: lirr
# Last edit: 01 Jul 2026
# R version 4.5.2

# note: 
    # LLM was used for code assistance
    # dataset last downloaded on: 01 July 2026

# assumes:


# does:
    # cleans data sets downloaded from project_DVC
    # harmonizes with manufacturing data

########################################################################
# - 0 Setup
########################################################################

# data file path
data <- file.path(
    "","Users", "Izz2", "Library", "CloudStorage",
    "OneDrive-SharedLibraries-WestVirginiaUniversity/Freedom Enyetornye - Fame_fiber"
)

# list of packages to be downloaded or installed
packages <- c(
    "fst", "lubridate", "dplyr", "sf", "readr", "stringr", "ggplot2", "fixest",
    "did2s", "tmap", "data.table", "haven", "tigris", "RColorBrewer", "Cairo",
    "maps", "rnassqs", "arrow", "assertthat"
)

# check for missing packages 
missing <- packages[!(packages %in% rownames(installed.packages()))]    

# install missing packages
if (length(missing)) install.packages(missing)

# load packages from library
invisible(lapply(packages, library, character.only = TRUE))


########################################################################
# - 1 load cotton data
########################################################################

# get list of files from OneDrive
data_list <- list.files(
    path = file.path(
        data, "cotton_data"
    ),
    pattern = "\\.csv$",
    full.names = TRUE
)

# create master cotton data table
cotton <- lapply(data_list,fread)

# rename the sub tables to match the file names
names(cotton) <- basename(data_list)

# subset gin counts from cotton
gin_counts <- cotton[["gin_counts.csv"]]

# subset harvested acres "
hrvstd_acrg <- cotton[["harvested_acres.csv"]]

# subset planted acres "
plntd_acrg <- cotton[["planted_acres.csv"]]

# subset production bales
bales <- cotton[["bales_produced.csv"]]

# subset sales
sales <- cotton[["sales.csv"]]

# subset yield
yield <- cotton[["yield.csv"]]


########################################################################
# - 2 load industry data
########################################################################

# create data object 
fib_naics <- data.table::fread(
    file = file.path(
        data, "master_data", "fiber_manufacturing_all.csv")
)


########################################################################
# - 3 clean data
########################################################################

# get identifying markers for each
