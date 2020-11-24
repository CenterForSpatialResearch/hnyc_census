###### Spell Check ######

# Required Libraries
library(dplyr)
library(hunspell)

# Load Raw Data
# rawData <- read_csv("...")

# `Street address 2` is the column for the Raw Street Address

#Rename key names
names(rawData) <- c("record", "year", "serial", "dwsize", "township", "pageno", "microseq", "n_fam", "splithid", "splitnum", "mcd", "county", "enumdist", "supdist", "street" )

#Parsing House Numbers
extract(rawData, "street", c("street_add", "house_num"), "(\\D+)(\\d.*)")

# Check Spelliing Function
checkSpelling <- function(rawData) {
  distinctStreetNames <- unique(rawData$`Street address 2`)
  misspeltStreets <- distinctStreetNames %>%
    hunspell() %>%
    unlist() %>%
    unique() %>%
    sort()
  print(misspeltStreets)
  print(paste0(length(misspeltStreets), " Possible Misspellings"))
}
