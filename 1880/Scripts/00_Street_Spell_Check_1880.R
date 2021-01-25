###### Spell Check ######

# Required Libraries
library(dplyr)
library(stringr)
library(hunspell)

# Load Raw Data
# rawData <- read_csv("...")

# Draw a test sample of 100
# rawData <- rawData[sample(nrow(rawData), 100), ]

# `Street address 2` is the column for the Raw Street Address

#Rename key names
names(rawData) <- c("record", "year", "dwelling_ser", "dwsize", "township", "pageno", "microfilm", "n_fam", "hh_ser_bef_split", "split", "mcd", "county", "ED", "supdist", "street")

#Parsing House Numbers
rawData <- extract(rawData, "street", c("house_num", "street_add"), "(\\d.)(\\D+)")

# Check Spelliing Function
checkSpelling <- function(rawData) {
  distinctStreetNames <- unique(rawData$street_add)
  misspeltStreets <- distinctStreetNames %>%
    hunspell() %>%
    unlist() %>%
    unique() %>%
    sort()
  print(misspeltStreets)
  print(paste0(length(misspeltStreets), " Possible Misspellings"))
}