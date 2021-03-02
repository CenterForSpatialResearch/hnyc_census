# Importing libraries into libPath, as Columbia HPC does not allow installation of
# packages into their storage space. COMMENT OUT if running on local computer.
.libPaths("../../../census/rpackages")
.libPaths() # To check if appending was successful

# Import all packages
library(hunspell)
library(tidyverse)
library(fuzzyjoin)
library(readr)
source("1880/Scripts/01_Street_Clean_Function_1880.R")
source("1880/Scripts/02_Street_Matching_1880.R")
source("1880/Scripts/03_Matched_Street_Fill_Down_1880.R")
source("1880/Scripts/04_HouseNumber_Clean_1880.R")
source("1880/Scripts/04_HN_Clean_1880.R")
source("1880/Scripts/05_House_Number_Fill_Down_1880.R")
source("1880/Scripts/06_Address_builder_1880.R")


# Set up to run for BK 1880 Census EDIT FOR EACH NEW RUN - currently for 1880 BK
# Import data
rawData <- read_csv("1880/census_1880_h_bk_sample100k.csv")
names(rawData) <- c("record", "year", "dwelling_ser", "dwsize", "township", 
                    "pageno", "microfilm", "n_fam", "hh_ser_bef_split", 
                    "split", "mcd", "county", "ED", "supdist", "street")
census_data <- extract(rawData, "street", c("house_num", "street_add"), "(\\d.)(\\D+)")
# Street Dictionary EDIT TO TOGGLE MN OR BK DICTS
edict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_street_dictionary/master/StreetsALL/Data/geo_dict_1880_bk.csv")
#hn_dict <- read_csv("https://raw.githubusercontent.com/CenterForSpatialResearch/hnyc_census/master/Street_Dict/combine_bk.csv") %>%
hn_dict <- read_csv("1880/Street_Dict/segment_1880_bk.csv") #%>%
#  select(Left_Low = Left_Low,
#        Left_High = Left_High,
#         Right_Low = Right_Low,
#        Right_High = Right_High,
#        everything()) %>%
# clean_hn_dict()
#Census Data EDIT INPUT CSV TO TOGGLE MN OR BK INPUT
#census_data <- read_csv("../Data/input/census_1910_h_bk.csv")
#census_data <- rawData


# Running all functions
output <- street_match(census_data, edict) %>%
  fillDownStreet(.) %>%
  house_clean(., edict, hn_dict) %>%
  fillDownHouseNum(.) %>%
  street_type_builder(.) %>%
  build_Address(.)

# Writing Output to CSV, COMMENT OUT if not needed EDIT to reflect MN BK and VERSION
#write_csv(output, "../Data/output/output_1880_bk_1st.csv")
write_csv(output, "1880/output_1880_bk_0125.csv")
