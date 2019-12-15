## Final Scripts
### Order of Functions
* Gerald's cleaning
* **01_Street_Clean_Function_MNBK.R**: [function] creates function that cleans street names
* **02_Street_Matching_MNBK.R**: [function] does address matching using Street Dictionary and filling down.
* **03_Matched_Street_Fill_Down.R**: [function + script] fills down `best_match` within an enuneration page and an ED. Note: in `\archive` folder there is an unoptimized version. The unoptimized version does all the same things, but runs matching twice, the 2nd time with adjusted ED scores. As this process takes longer with few differences in results, it is not used for now.
* **04_house_clean.R**: [function] Cleans house number: extracts house numbers from streets, separates modifiers and separates house ranges
* **05_House_Number_Fill_Down.R** [function + one-line script] simply fills down `house_num` within a street name and an ED.
* Bo's House No. Sequence Detection

### Run Full Script
An example of how to run all the scripts together is in **99_Full_Census_Cleaning**

## Documentation
Quick links to preview Documentation HTML files in the repo:

* [Census EDA](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Census_EDA_MN.html)
* [Street Cleaning (by Kyi)](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Cleaning_MN.html)
* [Street Matching MN 100k sample](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Dist_Check_MN.html)
* [Street Matching BK 100k sample](https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Dist_Check_BK.html)
* [House Number Cleaning](https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/House/House_Cleaning_100k_MN.html)