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
This project tracks how the balance between US cotton production and domestic mill use has shifted across California and Texas, and what the gap between them, visible both regionally and in the fiber embodied in imports, reveals about the challenges and opportunities facing the sector.
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

General imports (nominal dollars): USITC DataWeb

Access: (https://dataweb.usitc.gov/)

What it contains: Nominal dollar value of general imports for 
313, 314, and 315

### Labor and Establishment Data

Source: BLS Quarterly Census of Employment and Wages (QCEW)

Access: (https://www.bls.gov/cew/downloadable-data-files.htm)

Data dictionary: (https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm)

What it contains: Establishment counts and employment/wage data at the county or state level, NAICS-classified, for 313, 314 and 315. Regions include Textile belt : "North Carolina", "South Carolina", "Georgia", "Alabama", "Virginia"


## How to use code

First add your file to the lines in  project_DVC.R then run the file.
To recreate the manufacturing figure run : file.file
To recreate the maps run maps_code_new
To doodley bop bip badles


## Folder Structure

First run the cotton manufacturing file.
Then for ach set of raw files not in the folder included are downloaded by running Project_DVC.R.
Then run the
After running that file run the cotton harmonization file.
Then run the maps_





## Other Stuff?

[QCEW: Quarterly Census of Employment and Wages](https://www.bls.gov/cew/downloadable-data-files.htm)
[Dictionary](https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm)

