# Import all packages
library(hunspell)
library(tidyverse)
library(fuzzyjoin)
source("01_Street_Clean_Function_MNBK.R")
source("02_Street_Matching_MNBK.R")
source("03_Matched_Street_Fill_Down.R")
source("04_1_clean_hn_dict.R")
source("04_house_clean.R")
source("05_House_Number_Fill_Down.R")

# Import data
edict <- read_csv("full_mn_dict.csv")
sample <- read_csv("census_sample.csv")
hn_dict <- clean_hn_dict("combine_mn.csv")

# Load all functions
## To run all scripts

# Run scripts
output <- sample %>%
  street_match(., edict) %>% # from 02_Street_Matching
  fillDownStreet(.) %>% # from 03_Matched_Street_Fill_Down
  house_clean(., edict, hn_dict) %>% # from 04_house_clean
  fillDownHouseNum(.) # from 05_House_Number_Fill_Down
