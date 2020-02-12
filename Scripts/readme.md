## Final Scripts
### Order of Functions
* **00_Street_Spell_Check_MNBKR.R**: [function] runs a spell check for the street names on the raw data and prints out a list and count of possible misspellings. 
* **01_Street_Clean_Function_MNBK.R**: [function] creates function that cleans street names.
* **02_Street_Matching_MNBK.R**: [function] does address matching using Street Dictionary and filling down.
* **03_Matched_Street_Fill_Down.R**: [function + script] fills down `best_match` within an enuneration page and an ED. Note: in `\archive` folder there is an unoptimized version. The unoptimized version does all the same things, but runs matching twice, the 2nd time with adjusted ED scores. As this process takes longer with few differences in results, it is not used for now.
* **04_1_hn_dict_clean.R** [function] wrangles `combined_MN_dict.csv` to format needed by 04_house_clean.
* **04_house_clean.R**: [function] Cleans house number: extracts house numbers from streets, separates modifiers and separates house ranges
* **05_House_Number_Fill_Down.R** [function + one-line script] simply fills down `house_num` within a street name and an ED.
* Bo's House No. Sequence Detection

### Run Full Script
* Consolidated Scripts can be found in the `Full Run` folder
  * Separate scripts were made for Manhattan and Brooklyn
An example of how to run all the scripts together is in **99_Full_Census_Cleaning**

### Exploratory Data Analysis
* EDA can be found in the `EDA` folder within the `Full Run` folder

## Documentation
Quick links to preview Documentation HTML files in the repo:

* [Census EDA](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Census_EDA_MN.html)
* [Street Cleaning (by Kyi)](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Cleaning_MN.html)
* [Street Matching MN 100k sample](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Dist_Check_MN.html)
* [Street Matching BK 100k sample](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Dist_Check_BK.html)
* [House Number Cleaning](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/House/House_Cleaning_100k_MN.html)
* [House Number Sequence Detection Report](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/House/merge_sequence_report.html)
