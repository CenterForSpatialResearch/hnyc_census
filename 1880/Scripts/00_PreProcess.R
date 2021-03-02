#Pre Process 1880 Census HH Data

library(tidyverse)
source("1880/Scripts/01_Street_Clean_Function_1880.R")
source("1880/Scripts/05_House_Number_Fill_Down_1880.R")
source("1880/Scripts/06_Address_Builder_1880.R")

#Load Data (MN and BK)
census_1880_h_bk <- read_csv("Data/Input Data/census_1880_h_bk.csv") %>% 
    mutate(house_num = NA) %>% 
    select(rectype, year, serial, splithid, pageno, microseq, city, county, 'ED' = enumdist, supdist, house_num,  'street_add' = street)

census_1880_h_mn <- read_csv("Data/Input Data/census_1880_h_mn.csv") %>% 
    mutate(house_num = NA) %>% 
    select(rectype, year, serial, splithid, pageno, microseq, city, county, 'ED' = enumdist, supdist, house_num,  'street_add' = street)

# SET [DATA / SOURCE]
data <- census_1880_h_mn
# ---- Cleaning: Extract House No. + Modifiers ----

house_clean <- function(data) {

HN <- data %>%
    select(pageno, ED, 
           serial, house_num, street_add) %>%
    mutate(originalHN = house_num,
           street_add = ifelse(is.na(street_add), " ", street_add))

HN <- HN %>% rowwise() %>%
    mutate(flag1=ifelse(is.na(house_num)&
                            !is.na(str_extract(street_add,"([0-9]+\\s(&|-|TO)\\s[0-9]+\\s)|([0-9]+(R|A|B|C)+\\s)|([0-9]+\\.[5]\\s)|([0-9]+\\s)|(\\*)|(NA\\s*)|(na\\s*)")), 
                        str_extract(street_add,"([0-9]+\\s(&|-|TO)\\s[0-9]+\\s)|([0-9]+(R|A|B|C)+\\s)|([0-9]+\\.[5]\\s)|([0-9]+\\s)|(\\*)|(NA\\s*)|(na\\s*)"),
                        NA),
           house_num=ifelse(is.na(house_num), flag1, house_num),
           flg_hn_from_strt2=ifelse((!is.na(flag1))&(house_num != flag1) , 1, 0),
           flg_hn_from_strt=ifelse(is.na(flag1), 0, 1),
           house_num = gsub("\\s*(TO)\\s*", "-", house_num, ignore.case = TRUE)) %>%
    select(-flag1) %>% 
    mutate(street_name = str_trim(str_remove(street_add,"([0-9]+\\s(&|-|TO)\\s[0-9]+\\s)|([0-9]+(R|A|B|C)+\\s)|([0-9]+\\.[5]\\s)|([0-9]+\\s)|(\\*)|(NA\\s*)")))

    # Separate modifiers into new column
    HN <- HN %>% rowwise() %>%
        mutate(
               originalHN = house_num,
               modifier.number = str_extract(house_num, "\\s1[/-]2\\b"), 
               house_num = gsub("\\s1[/-]2\\b", " ", house_num),  
               house_num = gsub("\\s*(TO|/|&)\\s*", "-", house_num, ignore.case = TRUE), 
               house_num = gsub("(?<=\\d)\\s+(?=\\d)", "-", house_num, ignore.case = TRUE, perl = TRUE), 
               modifier.word = trimws(str_extract(house_num, "[A-Za-z\\s]+")),            
               house_num = gsub("\\s+", " ", house_num),                    
               house_num = trimws(gsub("[A-Za-z\\s]+", "", house_num)),
               house_num = gsub("^\\D+", "", house_num, ignore.case = TRUE), 
               house_num = gsub("\\D+$", "", house_num, ignore.case = TRUE), 
               flg_cleaned = ifelse(originalHN=="", 0, ifelse(house_num==originalHN, 0, 1))) 
    
    # ---- Cleaning: House Ranges ----
    # Cleaning house ranges with "-"
    hn_range_clean <- function(hn_range) {
        hn <- hn_range %>%
            str_split("-") %>% unlist()
        
        same_length <- str_length(hn) %>%
            unique() %>%
            length() == 1
        
        if (!same_length) {
            hn <- as.integer(hn)
            max_hn <- as.character(max(hn))
            min_hn <- as.character(min(hn))
            len_diff <- str_length(max_hn) - str_length(min_hn)
            
            clean <- map_chr(hn, function(x) ifelse(str_length(x) < str_length(max_hn), 
                                                    ifelse(str_length(paste0(str_sub(max_hn, 1, len_diff), x)) == str_length(max_hn),
                                                           paste0(str_sub(max_hn, 1, len_diff), x),
                                                           x),
                                                    x)
            )
            
            hn_range <- str_c(clean, collapse = "-")
            
        }
        
        return(hn_range)
    }

    
    HN <- HN %>%
        mutate(hn_range = hn_range_clean(house_num)) %>%
        mutate(flg_cleaned = ifelse(house_num == hn_range, 0, 1)) 
    
    splt_df <- str_split_fixed(HN$hn_range, pattern = "-", n = 3) %>% data.frame() %>% 
        rename(hn_1 = X1, hn_2 = X2, hn_3 = X3)# %>% 
        #mutate_all(as.character) %>% mutate_all(as.numeric)
    
    HN <- HN  %>% 
        cbind(splt_df)
   
    # ---- Output ----
    # [FLAG]: choose variables
    HN <- HN %>%
        select(ED, serial, street_add, originalHN, house_num,
               modifier.number, modifier.word,
               hn_1, hn_2, hn_3, street_name, flag_hn_cleaned = flg_cleaned) %>%
        distinct(ED, serial, street_add, .keep_all = TRUE)
    
    data <- left_join(data, HN,
                        by = c("ED", "serial", "street_add")) %>% 
        select(-house_num.x, "house_num" = house_num.y)
    
    return(data) 
}

#Run Street Clean Function

data <- data %>% 
    mutate(street_name_clean = clean(street_name))

# Fill in HN and Street Name where * or ""

data_filled <- fillDownHouseNum(data)

data_filled <- data %>% 
    mutate(across(where(is.character), ~na_if(., ""))) %>% 
    filter(rectype == "H") %>%
    mutate(house_num_temp = house_num) %>% 
    group_by(ED, street_name_clean) %>%
    fill(modifier.number, modifier.word, house_num, hn_1, hn_2, hn_3, .direction="down") %>%
    fill(modifier.number, modifier.word, house_num, hn_1, hn_2, hn_3, .direction="up") %>%
    rowwise() %>% 
    mutate(flg_filled_hn = ifelse(!is.na(house_num)  && is.na(house_num_temp), 1, 0)) %>% 
    select(- house_num_temp) %>%
    ungroup()

# Run Address Builder function
data_processed <- data_filled %>% 
    rename("best_match" = street_name_clean) %>% 
    street_type_builder(.) %>% 
    build_Address(.)
    

#---- Write CSV ---  [FLAG]:edit name to reflect date and borough
write_csv(data_processed, "census_1880_h_mn_20210225_processed.csv")

