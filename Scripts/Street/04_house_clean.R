
#' 04 Clean House Numbers
#' 
#' @param sample Census sample of house numbers to be cleaned (does not require "H" person filling)
#' @param edict Street Dictionary used in joining process
#' @param hn_dict Street Dictionary with House Numbers (as output of hn_dict_clean)
#' 
#' @output Original Census data + additional columns from 02_Street_Matching function (\code{street_match()}) + 5 new columns:
#' @description {
#'   \item{modifier.number} Modifying house number numbers, e.g. 1/2
#'   \item{modifier.word} Modifying house number words, e.g. REAR
#'   \item{hn_1} First house number.
#'   \item{hn_2} Second house number (if house number range)
#'   \item{hn_3} Third house number (if house number range)
#'   \item{flag_hn_cleaned} Whether the house number has been cleaned, e.g. modifiers removed or split as house range
#' }
#' 
#' The original house number is stored in \code{house_num}
#' 

house_clean <- function(sample, edict, hn_dict) {
  
  # ---- Preprocessing ----
  # select only records with house numbers

  HN <- sample %>%
    fill(microfilm, .direction = "up") %>%
    filter(!is.na(house_num)) %>%
    select(microfilm, ED, 
           dwelling_ser, house_num, street_add, best_match) %>%
    mutate(originalHN = house_num,
           house_num = ifelse(house_num=="", NA, as.character(house_num)),
           street_add = ifelse(is.na(street_add), " ", street_add))
  
  # ---- Cleaning: Extract House No. + Modifiers ----
  # Extract house number from street
  HN <- HN %>% rowwise() %>%
    mutate(flag1=ifelse(is.na(house_num)&
                          !is.na(str_extract(street_add,"[0-9]+\\s*(-|TO)+\\s*[0-9]+")), 
                        str_extract(street_add,"[0-9]+\\s*(-|TO)+\\s*[0-9]+"),
                        NA),
           house_num=ifelse(is.na(house_num), flag1, house_num),
           flg_hn_from_strt2=ifelse((!is.na(flag1))&(house_num != flag1) , 1, 0),
           flg_hn_from_strt=ifelse(is.na(flag1), 0, 1),
           house_num = gsub("\\s*(TO)\\s*", "-", house_num, ignore.case = TRUE)) %>%
    select(-flag1)
  
  # Separate modifiers into new column
  HN <- HN %>% rowwise() %>%
    mutate(modifier.number = str_extract(house_num, "\\s1[/-]2\\b"), 
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
    rename(hn_1 = X1, hn_2 = X2, hn_3 = X3) %>% 
    mutate_all(as.character) %>% mutate_all(as.numeric)
  
  HN <- HN  %>% 
    cbind(splt_df)
  
  # Additional cleaning of large house numbers that are ranges
  HN <- HN %>%
    left_join(hn_dict, by = c("ED" = "ED", "best_match" = "Name"))
  
  large_HN <- HN %>%
    ungroup() %>%
    mutate(within = ifelse(hn_1 < Low | hn_1 > High, FALSE, TRUE)) %>%
    mutate(range = paste0(Low, "-", High)) %>%
    group_by(ED, best_match, hn_1) %>%
    summarize(n_out = sum(within), out_range = TRUE) %>%
    filter(n_out == 0) %>%
    select(- n_out)
    
  HN <- HN %>%
    left_join(large_HN, by = c("ED", "best_match", "hn_1")) %>%
    mutate(out_range = ifelse(is.na(out_range), FALSE, out_range),
           Low = ifelse(is.na(Low), 0, Low),
           High = ifelse(is.na(High), 0, High)) %>%
    mutate(hn1_first_2 = as.integer(str_sub(house_num, 1, 2)),
           hn1_last_2 = as.integer(str_sub(house_num, str_length(house_num) - 1, str_length(house_num)))) %>%
    mutate(hn_1 = ifelse(out_range
                         & str_length(house_num) != str_length(Low) 
                         & str_length(house_num) != str_length(High)
                         & hn1_first_2 >= Low & hn1_first_2 <= High
                         & hn1_last_2 >= Low & hn1_last_2 <= High,
                         hn1_first_2, hn_1),
           hn_2 = ifelse(out_range
                         & str_length(house_num) != str_length(Low)
                         & str_length(house_num) != str_length(High)
                         & hn1_first_2 >= Low & hn1_first_2 <= High
                         & hn1_last_2 >= Low & hn1_last_2 <= High,
                         hn1_last_2, hn_2),
           flg_cleaned = ifelse(!is.na(hn_2), 1, 0))

  # ---- Output ----
  # [FLAG]: choose variables
  HN <- HN %>%
    select(ED, dwelling_ser, originalHN, best_match, 
           modifier.number, modifier.word,
           hn_1, hn_2, hn_3, flag_hn_cleaned = flg_cleaned) %>%
    distinct(ED, dwelling_ser, originalHN, best_match, .keep_all = TRUE)
  
  sample <- left_join(sample, HN,
                      by = c("ED", "dwelling_ser", "best_match",
                             "house_num" = "originalHN"))
  
  return(sample)
}
