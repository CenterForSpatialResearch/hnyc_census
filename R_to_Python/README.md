### Order of Functions 
* **00_Street_Spell_Check_MNBKR.ipynb**: [function] runs a spell check for the street names on the raw data and prints out a list and count of possible misspellings.
* **01_Street_Clean_Function_1880.ipynb**: [function] creates function that cleans street names.
* **02_Street_Matching_MNBK.ipynb**: [function] does address matching using Street Dictionary and filling down.
* **03_Matched_Street_Fill_Down.ipynb**: [function + script] fills down `best_match` within an enuneration page and an ED. Note: in `\archive` folder there is an unoptimized version. The unoptimized version does all the same things, but runs matching twice, the 2nd time with adjusted ED scores. As this process takes longer with few differences in results, it is not used for now.
* **04_1_hn_dict_clean.ipynb**: [function] wrangles combined_MN_dict.csv to format needed by `04_house_clean`.
* **04_HN_Clean_1880.ipynb** [function] Cleans house number: extracts house numbers from streets, separates modifiers and separates house ranges.
* **05_House_Number_Fill_Down.ipynb**: [function] simply fills down `house_num` within a street name and an ED.

