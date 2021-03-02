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
source("03_Matched_Street_Fill_Down_1880.R")
source("04_HouseNumber_Clean_1880.R")
source("04_HN_Clean_1880.R")
source("05_House_Number_Fill_Down_1880.R")
source("06_Address_builder_1880.R")


# Codes below need to be updated for 1880 census data

# Set up to run for BK 1910 Census EDIT FOR EACH NEW RUN - edited 20200604 BK
# Import data
# Street Dictionary EDIT TO TOGGLE MN OR BK DICTS
edict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/1880/Street_Dict/full_bk_dict1880.csv")
hn_dict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/1880/Street_Dict/combine_bk1880.csv") %>%
  select(Left_Low = y1880Left_Low,
         Left_High = y1880Left_High,
         Right_Low = y1880Right_Low,
         Right_High = y1880Right_High,
         everything()) %>%
  clean_hn_dict()
#Census Data EDIT INPUT CSV TO TOGGLE MN OR BK INPUT
#census_data <- read_csv("../Data/input/census_1910_h_bk.csv")
census_data <- rawData


# Running all functions
output <- street_match(census_data, edict) %>%
  fillDownStreet(.) %>%
  house_clean(., edict, hn_dict) %>%
  fillDownHouseNum(.) %>%
  street_type_builder(.) %>%
  build_Address(.)

# Writing Output to CSV, COMMENT OUT if not needed EDIT to reflect MN BK and VERSION
#write_csv(output, "../Data/output/output_1880_bk_1st.csv")
write_csv(output, "1880/output_1880_bk_1st.csv")
