# HNYC - Street_clean_functions

Working with 1910 Census microdata and preparing it for geocoding

**clean_street_final_v03.ipynb:** Clean the street addresses with some typical addresses, e.g. EAST 107TH STREET, AVENUE B, EAST 38, etc
**clean_street_final_v04.ipynb:** Clean the street addresses with small 2k sample 1910 Manhattan dataset.
**clean_street_final_v05.ipynb:** Optimize some special cases in ***clean_street_final_v04***.  
**clean_street_final_v06.ipynb:** Extract different components from final street clean address. e.g. final_street_address_clean: E 107 ST; Street_direction:'E'; Street_name:'107'; Street_type:'ST'  
**clean_street_final_v06_test_class.ipynb:** Tested the runtime for the function in class Street.
**clean_street_final_v06_test_functions.ipynb:** Tested the runtime of functions not in class to compare the difference of the runtime in class or not in class.
**clean_street_final_v06_functions_documentation.ipynb:** Write the documentation for the clean_street_final_v06.
**clean_street_final_v07.ipynb:** Modified on ***clean_street_final_v06***. This modification means that the final dataset would not contain the cleaning process for each step and just apply the function on the 'Street address 2' column.  
**clean_street_final_v08.ipynb:** Modified on ***clean_street_final_v07***. This modification just merges all the different functions into one clean function and applies to the 'Street address 2' column. This would not display different parts of the street addresses and only return the final clean street column.

#### Reasons for these modification: Since the runtime for Python functions takes 9mins to clean the addresses, which is much slower than in R. I wonder if it is related to the number of functions, I want to optimize the run time for the functions. The clean_street_final_v08.ipynb takes less time to run.