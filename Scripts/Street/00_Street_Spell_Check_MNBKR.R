###### Spell Check ######

# Required Libraries
library(dplyr)
library(hunspell)

# Load Raw Data
# rawData <- read_csv("...")

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