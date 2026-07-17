# Project: 2026 data viz challenge 
# Created on: 16 July 2026
# Created by: fe, si, lirr
# Edited by: lirr
# Last edit: 16 Jul 2026
# R version 4.5.2

# note: 
    # LLM was used for code assistance
    # original dataset downloaded on: 16 July 2026

# assumes:
    # have cloned repository
    # folder structure has not changed


# does:
    # sets up file paths
    # downloads raw data


########################################################################
# - 0 Setup
########################################################################


########################################################################
# - 0 (a) Setup - packages
########################################################################

# list of packages
packages <- c(
    "arrow", "assertthat", "Cairo", "countrycode" ,"cowplot" , "data.table",
    "did2s", "dplyr", "fixest", "fst", "ggplot2", "haven", "here", "lubridate",
    "maps", "patchwork", "RColorBrewer", "readr", "readxl" ,"rnassqs", "scales",
    "sf", "stringi","stringr", "tidyverse", "tigris", "tmap", "vroom"
)

# check for missing packages 
missing <- packages[!(packages %in% rownames(installed.packages()))]

# install missing packages
if (length(missing)) install.packages(missing)

# load packages from library
invisible(lapply(packages, library, character.only = TRUE))


########################################################################
# - 0 (b) Setup - file paths
########################################################################

# get local working directory
root <- file.path(".", "fiber_crops")

# code file path
code <- file.path(root, "code")

# raw data path relative to wd
raw <- file.path(root, "raw")

# api folder create
api_input <- file.path(root, "api_input")

# manufacturing data path
manufacturing <- file.path(raw, "manufacturing")

# cotton data path
cotton <- file.path(raw, "cotton")

# figures data path 
fig <- file.path(root, "figures")

# refined data path
refined <- file.path(root, "refined")

# check for directory existence if not then create
if (!dir.exists(raw)) {
        dir.create(raw, recursive = TRUE)
} else {
    message("Already exists")
}

if (!dir.exists(api_input)) {
    
    #vector of allowed responses
    allowed <- c("y", "n")
    
    repeat{
        
    # prompt user about usda api code
    ans <- tolower( # response remains lower case
        readline(
            prompt = "Do you have an API to access NASS's Quick Stats? (y/n): "
        )
    )
    
    # check for valid choice
    if (ans %in% allowed) {
        
        # moves on when answer is y
        if (ans == "y") {
            
            # create directory
            dir.create(api_input, recursive = TRUE)
            # ask to input api
            api < toString(
                readline(
                    prompt = "Please input api: "
                )
            )
            
            # check if valid api  (ASSUMES APIs are all 36 characters including dashes)
            if ( nchar(api) == 36) {
                
                # create file and folder path
                writeLines(api, con = file.path(api_input, "usda_api.txt"))
            
            } else { 
                warning("Please input valid api (may involve changing code around lines 114 in project~.R)")
                break
            }
        } else {
            warning("Please get api and save to file in new folder as follows ./api_input/usda_api.txt folder")
            break
        } 
    } else {
        cat("Invalid entry please type 'y' or 'n'. \n")
    }
    }
    
}

if (!dir.exists(manufacturing)) {
    dir.create(manufacturing, recursive = TRUE)
} else {
    message("Already exists")
}

if (!dir.exists(cotton)) {
    dir.create(cotton, recursive = TRUE)
} else {
    message("Already exists")
}

if (!dir.exists(fig)) {
    dir.create(fig, recursive = TRUE)
} else {
    message("Already exists")
}

if (!dir.exists(refined)) {
    dir.create(refined, recursive = TRUE)
} else {
    message("Already exists")
}

if (!dir.exists(file.path(cotton, "ers"))) {
    dir.create( file.path(cotton, "ers"))
} else {
    message("Already exists")
}


if (!dir.exists(file.path(cotton, "nass"))) {
    dir.create(file.path(cotton, "nass"))
} else {
    message("Already exists")
}


########################################################################
# - 1 run files that download data
########################################################################

# checks for cleaned manufacturing csv, !exist then downloads data
if (!file.exists(file.path(refined, "manufacturing","fiber_manufacturing_all.csv"))) { 
    
    #create valid answer
    allowed <- c("y", "n")
    
    # loop to get correct response
    repeat {
    # prompt due to time and data it takes to download
        ans <- tolower(
            readline(
                prompt = "Data takes a long time to download and is many gigabytes. Do you want to proceed (y/n): "
            )
        )
        # check for valid choice
        if (ans %in% allowed) {
            
            # Executes download when answer is "y"
            if (ans == "y") {
            
            # NOTE files are very large download at risk this code for easy reproducibility
                source(file.path(code, "enyetornye", "manufacturing_data_download.R"))
            } else {
                warning( "Download skipped missing files may cause code to stop. See README")
        }
            break # 
        }
    
    # error message
        cat("Invalid entry, please type 'y' or 'n'.\n")
    }
    

    
} else {
    message("Files unneeded or skipped")
}

# print continuing script acknowledgement
print("Script continues...")

# USDA ERS cotton data US scale usage imports exports 
if (!file.exists(file.path(cotton, "ers","us-cotton-supply-and-demand.csv"))) {
    source(file.path(code, "ikeme", "ERS_cotton.R"))
} else {
    message("Files unneeded")
}

# nass quickstats cotton data county level production and acreage

# get list of files in folder
cttn_csvs <- list.files(
    file.path(cotton, "nass")
)

# there should be 6 files name from nass downloads
if (length(cttn_csvs) < 6 ) {
    source(file.path(code, "rudinrush", "nass_download.R"))
} else {
    message("Files already exist, please check names")
}


########################################################################
# - 2 run files that clean data
########################################################################

# checks for existence of files and directories for refined manufacturing data
if (!dir.exists(file.path(refined, "manufacturing"))) {

    # create directory
    dir.create(file.path(refined, "manufacturing"))
    
    #if it doesnt exist run cleaning file
    source(file.path(code, "enyetornye", "manufacturing_data_cleaning.R"))
    
    # if directory exists check for file existence
} else if (!file.exists(file.path(refined,"manufacturing","fiber_manufacturing_all.csv"))) {
    
    # if file doesnt exist then it runs cleaning code assuming manufacturing data has been downloaded
    source(file.path(code, "enyetornye", "manufacturing_data_cleaning.R"))
    
    # if we get here all files are ok
} else {
    message("File already exists")
}

# list of files to check
clean_cttn_csvs <- list.files(file.path(refined, "cotton"))

# check for files and directories of cleaned cotton data
if (!dir.exists(file.path(refined, "cotton"))) {
    
    # create directory
    dir.create(file.path(refined, "cotton"))

    # run cleaning file for nass cotton data
    source(file.path(code, "rudinrush", "cotton_harmonization.R"))
    
} else if (length(clean_cttn_csvs) != 7) {
    
    source(file.path(code, "rudinrush", "cotton_harmonization.R"))
    
} else {
    
    message("Files already exist")
    
}

# check for production use file for map data
if (!file.exists(file.path(refined, "cotton_prod_use.csv"))) {
    source(file.path(code, "rudinrush", "data_clean_map.R"))
} else {
    message("File already exists")
}


########################################################################
# - 3 run files that create figures 
########################################################################

# check for existence of figure file if doesn't exist run file
if (!file.exists(file.path(fig, "trade_activity_combined.png"))) {
    source(file.path(code, "enyetornye", "manufacturing_trend_2.R"))
} else {
    message("File already exists")
}

# check for existence of map
if (!file.exists(file.path(fig, "pma_cali_ELS_chg_area.png"))) {
    source(file.path(code, "rudinrush", "maps_code_n.R"))
} else {
    message("File already exists")
}


