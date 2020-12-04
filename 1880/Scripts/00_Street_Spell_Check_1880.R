###### Spell Check ######

# Required Libraries
library(dplyr)
library(hunspell)

# Load Raw Data
# rawData <- read_csv("...")

# `Street address 2` is the column for the Raw Street Address

#Rename key names
names(rawData) <- c("record", "year", "dwelling_ser", "dwsize", "township", "pageno", "microfilm", "n_fam", "hh_ser_bef_split", "split", "mcd", "county", "ED", "supdist", "street")

#Parsing House Numbers
extract(rawData, "street", c("street_add", "house_num"), "(\\d.)(\\D+)")

# Check Spelliing Function
checkSpelling <- function(rawData) {
  distinctStreetNames <- unique(rawData$street)
  misspeltStreets <- distinctStreetNames %>%
    hunspell() %>%
    unlist() %>%
    unique() %>%
    sort()
  print(misspeltStreets)
  print(paste0(length(misspeltStreets), " Possible Misspellings"))
}
