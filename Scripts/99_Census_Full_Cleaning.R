# Importing libraries into libPath, as Columbia HPC does not allow installation of
# packages into their storage space. COMMENT OUT if running on local computer.
.libPaths("../rpackages")
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
edict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Street_Dict/full_mn_dict.csv")
hn_dict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Street_Dict/combine_mn.csv") %>%
  select(Left_Low = y1910Left_Low,
         Left_High = y1910Left_High,
         Right_Low = y1910Right_Low,
         Right_High = y1910Right_High,
         everything()) %>%
  clean_hn_dict()
sample <- read_csv("../data/input/census_sample.csv")


# Running all functions
output <- street_match(sample, edict) %>%
  fillDownStreet(.) %>%
  house_clean(., edict, hn_dict) %>%
  fillDownHouseNum(.) %>%
  street_type_builder(.) %>%
  build_Address(.)

# Writing Output to CSV, COMMENT OUT if not needed
write_csv(output, "../data/output/output_0505.csv")