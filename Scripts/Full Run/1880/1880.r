library(tidyverse)
source("Scripts/01_Street_Clean_Function_MNBK.R")
source("Scripts/06_Address_Builder.R")

### Load 1880 Census Data
census_1880_h_mn <- read_csv("Data/Input Data/census_1880_h_mn_split.csv")
census_1880_h_bk <- read_csv("Data/Input Data/census_1880_h_bk_split.csv")

test <- census_1880_h_bk %>% 
    group_by(dwelling_ser) %>% 
    tally()

### Set Census Data Input
census_data <- census_1880_h_bk %>% 
    mutate(street_addr = clean(toupper(street_add))) %>% 
    select('ser' = X1, dwelling_ser, 'hh_serial' = hh_ser_bef_split, year, pageno, microfilm, ED, 'hn_1' = house_nr, street_add, 'best_match' = street_addr)

output <- street_type_builder(census_data) %>% 
    street_type_builder(.) %>%
    build_Address(.)


###Export
##MN
write_csv(output, "census_1880_h_mn_output.csv")
##BK
write_csv(output, "census_1880_h_bk_output.csv")



