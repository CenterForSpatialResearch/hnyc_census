# Importing libraries into libPath, as Columbia HPC does not allow installation of
# packages into their storage space. COMMENT OUT if running on local computer.
.libPaths("/rigel/home/cxw2002/rpackages") # Ammend "cxw2002" to user account 
.libPaths() # To check if appending was successful

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
source("06_Address_builder.R")

# Import data
edict <- read_csv("full_mn_dict.csv")
sample <- read_csv("census_sample.csv")
hn_dict <- clean_hn_dict("combine_mn.csv")


# Running all functions
output <- street_match(sample, edict) %>%
  fillDownStreet(.) %>%
  house_clean(., edict, hn_dict) %>%
  fillDownHouseNum(.) %>%
  street_type_builder(.) %>%
  build_Address(.)

# Writing Output to CSV, COMMENT OUT if not needed
write_csv(output, "output.csv")