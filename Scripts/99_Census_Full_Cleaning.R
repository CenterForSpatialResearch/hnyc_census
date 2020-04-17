# Importing libraries into libPath, as Columbia HPC does not allow installation of
# packages into their storage space. COMMENT OUT if running on local computer.
.libPaths("/rigel/home/cxw2002/rpackages") # Ammend "cxw2002" to user account 
.libPaths()

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

# Load all functions
## To run all scripts. Sys.time() is used on the HPC to evaluate function run time.
## A later iteration would likely remove Sys.time once code has been evaluated.

# 1. Street Match
currentTime = Sys.time()
output <- street_match(sample, edict)
Sys.time() - currentTime
currentTime = Sys.time()

# 2. Filling Down Street Number
output <- fillDownStreet(output)
Sys.time() - currentTime
currentTime = Sys.time()

# 3. Cleaning House Number
output <- house_clean(output, edict, hn_dict)
Sys.time() - currentTime
currentTime = Sys.time()

# 4. Filling Down House Number
output <- fillDownHouseNum(output)
Sys.time() - currentTime
currentTime = Sys.time()

# 5. Appending Street Type
output <- street_type_builder(output)
Sys.time() - currentTime
currentTime = Sys.time()

# 6. Constructing Completed Address
output <- build_Address(output)
Sys.time() - currentTime
currentTime = Sys.time()

# 7. Writing Output to CSV, COMMENT OUT if not needed
write_csv(output, "output.csv")
Sys.time() - currentTime
