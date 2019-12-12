Order of Scripts
* Gerald's cleaning
* **01_Street_Clean_Function_MNBK.R**: [function] creates function that cleans street names
* **02_Street_Matching_MNBK.R**: [script] does address matching using Street Dictionary and filling down. Outputs the input census sample dataframe with `best_match` (matched street name), `result_type` (type of match - refer to `Street_Dist_Check` documentation), flag (whether the match was through fill down)
* **03_Matched_Street_Fill_Down.R** [function + one-line script] fills down `best_match` within an enuneration page and an ED.
* Potentially House No. Cleaning
* **05_House_Number_Fill_Down.R** [function + one-line script] simply fills down `house_num` within a street name and an ED.
* Bo's House No. Sequence Detection


Quick links to preview HTML files in the repo:

* [Street_Cleaning_MN](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Cleaning_MN.html)
* [Census_EDA_MN](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Census_EDA_MN.html)
* [Street_Dist_Check_MN](http://htmlpreview.github.io/?https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Scripts/Street/Street_Dist_Check_MN.html)
