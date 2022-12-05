## Street_clean

**street_direction.py**: This function aims to clean the street directions in the street addresses. For example, turn the 'WEST' to 'W', 'EAST' to 'E', etc.  
**street_type.py**: This function aims to clean the different format of street types in the street addresses. For example, turn the 'STREET', 'STREE' to 'ST'; 'PLACE' to 'PL', etc.  
**hn_street_split.py**: This function aims to split the house numbers and the street addresses. The input should be the original street addresses with house numbers, and the output should be street addresses. This function should be apply after the former cleaning steps because in some datasets the house numbers are included while in others they are excluded and started like '38 STREET', if I used this function first, the 38 would be omitted from the street addresses.  
**street_number_name.py**: This function aims to clean the different format of street numbers in the street addresses. For example, turn the 'E 108TH ST' to 'E 108 ST'; 'W NINTY-NINE ST' to 'W 99 ST', etc.  
**street_name.py**: This function aims to clean the different format of street names in the street addresses. For example, turn the 'WASH SQ' to 'WASHINGTON SQ', etc.  
**special_cases.py**: This function aims to clean some special cases which are not cleaned in the former functions in the street addresses. For example, turn the '4 W ST' to 'W 4 ST', etc.  
**street_clean.py**: This function aims to build a function which use dataframe and column as the input, and apply all the functions to the column to clean the addresses and get the final clean street addresses as outputs.  