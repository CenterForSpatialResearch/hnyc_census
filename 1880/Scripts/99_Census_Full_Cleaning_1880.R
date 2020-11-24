# Importing libraries into libPath, as Columbia HPC does not allow installation of
# packages into their storage space. COMMENT OUT if running on local computer.
.libPaths("../../../census/rpackages")
.libPaths() # To check if appending was successful

# Import all packages
library(hunspell)
library(tidyverse)
library(fuzzyjoin)
source("01_Street_Clean_Function_1880.R")
source("02_Street_Matching_1880.R")
source("03_Matched_Street_Fill_Down.R")
source("04_HouseNumber_Clean_1880.R")
source("04_HN_Clean_1880.R")
source("05_House_Number_Fill_Down_1880.R")
source("06_Address_builder_1880.R")


# Codes below need to be updated for 1880 census data

# Set up to run for BK 1910 Census EDIT FOR EACH NEW RUN - edited 20200604 BK
# Import data
# Street Dictionary EDIT TO TOGGLE MN OR BK DICTS
edict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Street_Dict/full_bk_dict.csv")
hn_dict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Street_Dict/combine_bk.csv") %>%
  select(Left_Low = y1910Left_Low,
         Left_High = y1910Left_High,
         Right_Low = y1910Right_Low,
         Right_High = y1910Right_High,
         everything()) %>%
  clean_hn_dict()
#Census Data EDIT INPUT CSV TO TOGGLE MN OR BK INPUT
census_data <- read_csv("../Data/input/census_1910_h_bk.csv")


# Running all functions
output <- street_match(census_data, edict) %>%
  fillDownStreet(.) %>%
  house_clean(., edict, hn_dict) %>%
  fillDownHouseNum(.) %>%
  street_type_builder(.) %>%
  build_Address(.)

# Writing Output to CSV, COMMENT OUT if not needed EDIT to reflect MN BK and VERSION
write_csv(output, "../Data/output/output_bk_0604.csv")
