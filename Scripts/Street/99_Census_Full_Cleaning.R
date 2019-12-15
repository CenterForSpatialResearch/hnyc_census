# Import all packages
library(hunspell)
library(tidyverse)
library(fuzzyjoin)

# Import data
edict <- read_csv("full_mn_dict.csv")
sample <- read_csv("census_sample.csv")
hn_dict <- read_csv("combined_edict_mn.csv") %>%
  clean_hn_dict(.) # from 04_1_clean_hn_dict

# Load all functions
## To run all scripts

# Run scripts
output <- sample %>%
  street_match(., edict) %>% # from 02_Street_Matching
  fillDownStreet(.) %>% # from 03_Matched_Street_Fill_Down
  house_clean(., edict, hn_dict) %>% # from 04_house_clean
  fillDownHouseNum(.) # from 05_House_Number_Fill_Down