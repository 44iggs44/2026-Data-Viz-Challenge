<!--
Project: 2026 data viz challenge - Fiber Crop Team
Created on: 15 Jul 2026
Created by: fe, si, lirr
Last edited by: lirr
Last edit: 16 Jul 2026
-->


# Fiber Crop Team: 
# 2026 USDA AMS/AAEA GSS Data Visualization Challenge

## Overview and Background:

The US cotton and textile sector has changed shape over recent decades.
The US remains one of the world's largest cotton producers and exporters, shipping most of its crop abroad, while domestic textile and apparel manufacturing has declined, and demand is now met largely through imports (about $107.7 billion in 2024).
This project tracks how the balance between US cotton production and domestic mill use in cotton has shifted across California and Texas, and what the gap between them, visible both regionally, reveals about the challenges and opportunities facing the sector.
This aligns with the Local and Regional Food Division's priorities on business viability and market access: mapping the locations of production and processing shows which regional businesses are most exposed and where domestic demand could be recaptured.

## Data Sources

### *Cotton Use Data National Level*

Source: USDA Economic Research Service (ERS)

Access: [Cotton Demand Data](https://www.ers.usda.gov/data-products/cotton-wool-and-textile-data/cotton-and-wool-yearbook)
 
<small><small>NOTE: link takes user to USDA ERS Wool and Cotton Year Book. Data can be downloaded using the file ERS_cotton.R or can be manually downloaded at the bottom of the page after following the link.</small></small>

What it contains: annual total cotton bales produced in thousands of bales, annual pima cotton production, annua upland cotton production, annual total domestic cotton usage in thousands of bales, annual domestic upland cotton usage in thousands of bales, annual domestic pima cotton usage in thousands of bales.

### *Cotton Production Data County Level*

Source: NASS Quickstats

Access: [Api code](https://quickstats.nass.usda.gov/api) is required.
After obtaining API key save a copy in usda_api.txt or input directly into the function nassqs_auth() before running the project_DVC.R file.

What it contains: County level cotton production data in acres and bales.

### *Textile Industry Output Data*

Value of shipments (nominal dollars): NBER-CES Manufacturing Database

Access: (www.nber.org/research/data/nber-ces-manufacturing-industry-database)

What it contains: Nominal dollar value of general output for 
313, 314, and 315

### *Textile Industry Import Data*

General imports (nominal dollars): USITC DataWeb

Access: (https://dataweb.usitc.gov/)

What it contains: Nominal dollar value of general imports for 
313, 314, and 315

### *Cotton Equivalent Imported Data*

Cotton equivalent imported from textile industries (pounds):USDA ERS

Access: (https://www.ers.usda.gov/data-products/cotton-wool-and-textile-data/raw-fiber-equivalents-of-us-textile-trade-data)

What it contains: Cotton equivalent imported from textile and apparel industries 

### Labor and Establishment Data

Source: BLS Quarterly Census of Employment and Wages (QCEW)

Access: (https://www.bls.gov/cew/downloadable-data-files.htm)

Data dictionary: (https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm)

What it contains: Establishment counts and employment/wage data at the county or state level, NAICS-classified, for 313, 314 and 315. Regions include Textile belt : "North Carolina", "South Carolina", "Georgia", "Alabama", "Virginia"

## To recreate the figures:

### First: Clone the repository and unzip the compressed file:

**The folder structure should look like this:**
```text
|___fiber_crops
| |___Archive.zip
| |___code
| | |___rudinrush
| | |___enyetornye
| | |___ikeme
| | |___project_DataViz_fiber_team.R
| |___enyetornye
| | |___code
| | |___figures
| | |___data
| |___README.md
```

### Then there are two options:

#### 1. Open the Archive.zip file or 2. Run the ```project_DataViz_fiber_team.R``` file

##### Option 1.

After opening the Archive.zip file there should be three folders labelled figures, raw, and refined.

The folder structure should **now** look like this:
```text
|___fiber_crops
| |___code
| | |___rudinrush
| | |___enyetornye
| | |___ikeme
| | |___project_DataViz_fiber_team.R
| |___enyetornye
| | |___code
| | |___figures
| | |___data
| |___README.md
| |___figures
| |___refined
| | |___cotton_prod_use.csv
| | |___cotton
| | |___manufacturing
| |___raw
| | |___cotton
| | | |___ers
| | | |___nass
| | |___textile_industry_output_import
| | |___manufacturing
```

To get the latest version of the figures please open and run the file named: ```text project_DataViz_filber_team.R``` in the code folder.
After running the text project_~_team.R file, the manufacturing data can be downloaded if updates to the data are desired.
Be aware that downloading all of the manufacturing data folders takes up around 4gb of space.


##### Option 2. Run the ```project_DataViz_fiber_team.R``` file.

Running this file will create the folders, download the files, clean the data, and output figures into the figures folder.

This will take up around 4gb of space and will take some time to complete.
There is a prompt that asks the user to confirm they want to download the manufacturing dataset.

The resulting folder structure should **now** look like this:

```text
|___fiber_crops
| |___code
| | |___rudinrush
| | |___enyetornye
| | |___ikeme
| | |___project_DataViz_fiber_team.R
| |___enyetornye
| | |___code
| | |___figures
| | |___data
| |___README.md
| |___figures
| |___refined
| | |___cotton_prod_use.csv
| | |___cotton
| | |___manufacturing
| |___raw
| | |___cotton
| | | |___ers
| | | |___nass
| | |___textile_industry_output_import
| | |___manufacturing
```


### Code Details

#### Folder structure of code

```text
| |___code
| | |___rudinrush
| | | |___nass_download.R
| | | |___cotton_harmonization.R
| | | |___data_clean_map.R
| | | |___maps_code_n.R
| | | |___old_code
| | | | |___state_period_comparison_map.R
| | | | |___code_graveyard.R
| | | | |___function_test.R
| | |___enyetornye
| | | |___manufacturing_trend.R
| | | |___manufacturing_data_download.R
| | | |___data_cleaning.R
| | |___ikeme
| | | |___ERS_cotton.R
| | | |___output_import_textile.R
| | |___project_DataViz_fiber_team.R
| | |___project_DVC.R
```

#### In-depth code explanations

**Lorin Rudin-Rush:**

The rudinrush folder contains code authored mainly by Lorin Rudin-Rush.
I adapted some code from Freedom and Freedom's map served as the basis for the aesthetics in both figures.
The nass_download.R file requires an API and there is a prompt that will ask to input your api.
This file downloads the county level cotton production data.
The cotton_harmonization file cleans the county level cotton production data, creates a clean .csv for each dataset, creats the index variable, and joins the cotton data with the other data sets.
The final data products are cotton_harmonized.csv which contains the county level production data and cttn_mnftr.csv which combines the production data with the manufacturing data.
The data_clean_map.R file combines the data sets with the demand data from the USDA fiber report.
The file also creates national level variables and year over year change variables which were not used.
The data product output is the file cotton_prod_use.csv saved to the refined data folder.
The maps_code_n.R file creates the california texas comparison visual.
It creates the index variable described in the write up.


**Freedom Enyetornye:**

manufacturing_data_download.R: downloads annual BLS QCEW manufacturing files for 1990–2025.
manufacturing_data_cleaning.R: selects the relevant textile and fiber-related NAICS industries and creates the harmonized manufacturing dataset.
manufacturing_trend.R: creates the visualization comparing textile output, imports, cotton-equivalent imports, employment, and establishments.

**Sionegael Ikeme:**

ERS_cotton.R: dowloads cotton ERS data (national supply, demand, as well as prices). The data also has in comment some quick data staticts and plots to visualize trends better.
output_import_textile.R: clean up data on textile naics 3 digits import and output data value at the industry level over time. The data contains as well in comment some quick data staticts and plots to visualize trends better.


## Other Stuff?

[QCEW: Quarterly Census of Employment and Wages](https://www.bls.gov/cew/downloadable-data-files.htm)
[Dictionary](https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm)



## AI Disclaimer

**Lorin Rudin-Rush:**
LLM Usage: 
I used generative AI for coding assistance.
Most of the time this was for quick debugging.
When I found that I was spending a lot of time on an problem or task, I would submit code and error to either antigravity or through the github copilot via VSCode or even google search.
When I was trying to get it to do larger tasks even with agentic models I ran into some issues.
I do not use much generative AI in my day-to-day workflow and the lack of experience in prompts may have limited the effectiveness for certain tasks.
On the other hand, after I created the state comparison maps in the R package tmap as a proof of concept.
I was having difficulties quickly translating the tmap code to ggplot2 code.
I copy and pasted the tmap code into a basic google search and asked it to match the formatting and style of Freedom's map created in the file manufacturing_trend_2.R.
Previous attempts at map translation can be seen in the old_code folder.
These were experiments and I was trying to find code that I could cannibalize to better learn ggplot2.
Using google antigravity and I believe codex/claude in github copilot, I got suggestions for code to build the map we agreed on.
I could not get these to work, I assume it was user error.

**Sionegael Ikeme:**
AI assistance was used in developing the code. Claude was used to fix lines of code that were causing errors, and to test API calls for downloading data directly from ERS, which was then rewritten to fit the context of the code.


