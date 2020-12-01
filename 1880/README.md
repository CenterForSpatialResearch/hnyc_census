## Final Scripts
### Order of Functions
* **00_Street_Spell_Check_1880.R**: [function] runs a spell check for the street names on the raw data and prints out a list and count of possible misspellings. 
* **01_Street_Clean_Function_1880.R**: [function] creates function that cleans street names.
* **02_Street_Matching_1880.R**: [function] does address matching using Street Dictionary and filling down.
* **03_Matched_Street_Fill_Down_1880.R**: [function + script] fills down `best_match` within an enuneration page and an ED. Note: in `\archive` folder there is an unoptimized version. The unoptimized version does all the same things, but runs matching twice, the 2nd time with adjusted ED scores. As this process takes longer with few differences in results, it is not used for now.
* **04_HN_clean_1880.R** [function] wrangles `combined_MN_dict.csv` to format needed by 04_house_clean.
* **04_HouseNumber_clean_1880.R**: [function] Cleans house number: extracts house numbers from streets, separates modifiers and separates house ranges
* **05_House_Number_Fill_Down_1880.R** [function + one-line script] simply fills down `house_num` within a street name and an ED.
* House No. Sequence Detection (under /House, 06_seqClean.R)


### Run Full Script
* Consolidated Scripts can be found in the `Full Run` folder
  * Separate scripts were made for Manhattan and Brooklyn
An example of how to run all the scripts together is in **99_Full_Census_Cleaning_1880**

## Exploratory Data Analysis
* [EDA](https://htmlpreview.github.io/?https://github.com/CenterForSpatialResearch/hnyc_census/blob/master/1880/Scripts/EDA/EDA_1880_Census.html) can be found in the `EDA` folder within the `Full Run` folder

## Documentation
Quick links to preview Documentation HTML files in the repo:

## Data, Inputs and Outputs
* Data, shapefiles, combined street dictionaries and output data can be found on the [google drive](https://drive.google.com/drive/u/1/folders/1lBPqmNGByQK41NQ3-dPQdfU3aGBLg5CS)
Data, Inputs and Outputs
Data, shapefiles, combined street dictionaries and output data can be found on the google drive
