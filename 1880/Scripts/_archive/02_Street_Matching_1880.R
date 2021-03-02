# This script contains the full street address matching and fill down as a function.

#' @param rawData Census sample to match. Note: this is currently tailored to samples that have same columns as the original 100k sample. If column names change, adapt the code to clean column names accordingly, ensuring that relevant column names retain the names specified in the function.
#' @param edict Street Dictionary used for matching. Ensure that each ED is a row and streets in the ED are in columns.
#' 
#' @output The full, original census sample data input into the function with 3 additional columns:
#' @description {
#'    \item{best_match} Column containing best street name match
#'    \item{result_type} 1 of 6 possible match types: (1) Perfect Match, (2) Identical Match, (3) Singular Mode, (4) Multiple Modes, (5) NA mode, (6) No match. Refer to documentation for more details.
#'    \item{flag_st} Flag if street name match was ultimately obtained via fill down (which is the case for result type 4-6).
#' }
#' 
#' The original street name is stored in \code{street_add}

# Libraries
library(tidyverse)
library(fuzzyjoin)
source("01_Street_Clean_Function_1880.R")

#use brooklyn street dictionary
#edict <- read_csv("1880/Street_Dict/full_bk_dict1880.csv")
#use manhattan street dictionary
#edict <- read_csv("1880/Street_Dict/full_mn_dict1880.csv")

# ---- FUNCTION ----
street_match <- function(rawData, edict) {
  ## clean column names: adapt code if sample has different columns. At minimum, ensure `record`, `ED` and `street_add` columns exist after cleaning.
  # names(sample) <- c("record", "township", "county", "ED", "person_id",
  #                    "dwelling_seq", "dwelling_ser", "dwelling_ser2", "hh_seq", "hh_seq_8",
  #                    "hh_ser2", "hh_ser_bef_split", "indiv_seq", "split", 
  #                    "line_no", "line_no2", "microfilm",
  #                    "n_fam", "n_person_bef_split", "house_num", "street_add")
  names(rawData) <- c("record", "year", "dwelling_ser", "dwsize", "township", 
                     "pageno", "microfilm", "n_fam", "hh_ser_bef_split", "split", 
                     "mcd", "county", "ED", "supdist", "house_num", "street_add")
  ## clean ED number
  edict <- edict %>%
    mutate(ED = str_pad(ED, 4, "left", pad = "0")) # ensure numbers are all 4 digit
  
  ## Extract Unique Addresses
  unique_addresses <- rawData %>%
    select(ED, street_add) %>%
    filter(!is.na(street_add)) %>%
    distinct(ED, street_add)
  
  # ---- CLEAN ADDRESSES ----
  ## import rem_dup_word
  rem_dup_word <- function(x){
    x <- tolower(x)
    paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = 
            " ")
  }
  
  ## [NOTE] make sure to run clean() function from '01_Street_Clean_Function_1880.R'
  
  ## preallocate memory for cleaned column
  cleaned <- rep(NA_character_, nrow(unique_addresses))
  
  ## apply rem_dup_word() and str_clean()
  for (i in 1:nrow(unique_addresses)) {
    cleaned[i] <- toupper(rem_dup_word(unique_addresses[i, 2]))
    cleaned[i] <- clean(cleaned[i])
  }
  
  # ---- FIRST MATCH ----
  ## create add_matches, which combines `unique_addresses` with the `cleaned` column
  add_matches <- tibble(ED = unique_addresses$ED,
                        raw = unique_addresses$street_add,
                        clean = cleaned)
  
  
  ## create str_algo, which has all the methods we will use
  str_algo <- c("dl", "qgram", "cosine", "jaccard", "jw")
  
  ## create empty lists which will store outputs
  algo_list <- list()
  ed_list <- list()
  
  ## matching
  for (i in unique(add_matches$ED)) {
    # create subset of edict and add_matches for a particular ED
    # TO ACCEPT BK: ED_dict <- filter(edict, ED == i & MNBK == "BK") %>%
    ED_dict <- filter(edict, ED == i) %>% 
      select(- "ED") %>%
      unlist() 
    ED_dict <- data.frame(clean = ED_dict, stringsAsFactors = FALSE) %>%
      filter(!is.na(clean))
    ED_add <- filter(add_matches, ED == i)
    
    for (j in str_algo) {
      # stringdist_join, extract best match for each method
      result <- stringdist_left_join(ED_add, ED_dict, by = "clean", 
                                     max_dist = 5, method = j, distance_col = "dscore") %>%
        select(ED, raw, clean.x, clean.y, dscore) %>%
        group_by(raw) %>%
        arrange(dscore) %>%
        slice(1)
      
      # at this point, result = dataframe of addresses in ED (i), merged with method (j)
      # append result to algo_list, each element in algo_list is a dataframe
      algo_list[[j]] <- result
      
      # "column bind" all elements in algo_list to form a df called ed_df
      # ed_df now contains all addresses in ED(i) with matches using all methods
      ed_df <- algo_list %>% 
        reduce(left_join, by = c("ED", "raw", "clean.x"))
    }
    # append each ed_df to ed_list 
    ed_list[[i]] <- ed_df
  }
  
  ## bind all dfs by row
  match_dict <- bind_rows(ed_list) %>%
    select(ED = ED, raw = raw, clean = clean.x,
           match_dl = clean.y.x, dscore_dl = dscore.x,
           match_qgram = clean.y.y, dscore_qgram = dscore.y,
           match_cos = clean.y.x.x, dscore_cos = dscore.x.x,
           match_jac = clean.y.y.y, dscore_jac = dscore.y.y,
           match_jw = clean.y, dscore_jw = dscore)
  
  ## ---- FURTHER TUNING ----
  ## create summary statistics
  summ_dscores <- match_dict %>%
    mutate(match_dl = ifelse(is.na(match_dl), "5", match_dl),
           match_qgram = ifelse(is.na(match_qgram), "5", match_qgram),
           match_cos = ifelse(is.na(match_cos), "5", match_cos),
           match_jac = ifelse(is.na(match_jac), "5", match_jac),
           match_jw = ifelse(is.na(match_jw), "5", match_jw),
           dscore_dl = ifelse(is.na(dscore_dl), 5, dscore_dl),
           dscore_qgram = ifelse(is.na(dscore_qgram), 5, dscore_qgram),
           dscore_cos = ifelse(is.na(dscore_cos), 5, dscore_cos),
           dscore_jac = ifelse(is.na(dscore_jac), 5, dscore_jac),
           dscore_jw = ifelse(is.na(dscore_jw), 5, dscore_jw)) %>%
    group_by(ED) %>%
    summarize(mean_dl = mean(dscore_dl), sd_dl = sd(dscore_dl),
              mean_qgram = mean(dscore_qgram), sd_qgram = sd(dscore_qgram),
              mean_cos = mean(dscore_cos), sd_cos = sd(dscore_cos),
              mean_jac = mean(dscore_jac), sd_jac = sd(dscore_jac),
              mean_jw = mean(dscore_jw), sd_jw = sd(dscore_jw)) %>%
    replace(is.na(.), 0)
  
  ## format into a list such that e.g. `summ_dscores_list$0010$jw$mean` gives mean jw dscore of ED 0010
  summ_dscores_list <- unique(summ_dscores$ED) %>% as.list()
  names(summ_dscores_list) <- unique(summ_dscores$ED)
  
  for (i in names(summ_dscores_list)) {
    s <- filter(summ_dscores, ED == i)
    ED_sum_list <- list(dl = list(mean = s$mean_dl, sd = s$sd_dl),
                        qgram = list(mean = s$mean_qgram, sd = s$sd_qgram),
                        cosine = list(mean = s$mean_cos, sd = s$sd_cos),
                        jaccard = list(mean = s$mean_jac, sd = s$sd_jac),
                        jw = list(mean = s$mean_jw, sd = s$sd_jw))
    summ_dscores_list[[i]] <- ED_sum_list
  }
  
  ## match again
  for (i in unique(add_matches$ED)) {
    # create subset of edict and add_matches for a particular ED
    # TO ACCEPT BK: ED_dict <- filter(edict, ED == i & MNBK == "BK") %>%
    ED_dict <- filter(edict, ED == i) %>% 
      select(- "ED") %>%
      unlist() 
    ED_dict <- data.frame(clean = ED_dict, stringsAsFactors = FALSE) %>%
      filter(!is.na(clean))
    ED_add <- filter(add_matches, ED == i)
    
    for (j in str_algo) {
      # set a max_dist that varies by ED. max_dist is 2sd higher than mean.
      threshold <- summ_dscores_list[[i]][[j]]$mean + 2 * summ_dscores_list[[i]][[j]]$sd
      
      # stringdist_join, extract best match for each method
      result <- stringdist_left_join(ED_add, ED_dict, by = "clean", 
                                     max_dist = threshold, method = j, distance_col = "dscore") %>%
        select(ED, raw, clean.x, clean.y, dscore) %>%
        group_by(raw) %>%
        arrange(dscore) %>%
        slice(1)
      
      # at this point, result = dataframe of addresses in ED (i), merged with method (j)
      # append result to algo_list, each element in algo_list is a dataframe
      algo_list[[j]] <- result
      
      # "column bind" all elements in algo_list to form a df called ed_df
      # ed_df now contains all addresses in ED(i) with matches using all methods
      ed_df <- algo_list %>% 
        reduce(left_join, by = c("ED", "raw", "clean.x"))
    }
    # append each ed_df to ed_list 
    ed_list[[i]] <- ed_df
  }
  
  ## bind all dfs by row
  match_dict <- bind_rows(ed_list) %>%
    select(ED = ED, raw = raw, clean = clean.x,
           match_dl = clean.y.x, dscore_dl = dscore.x,
           match_qgram = clean.y.y, dscore_qgram = dscore.y,
           match_cos = clean.y.x.x, dscore_cos = dscore.x.x,
           match_jac = clean.y.y.y, dscore_jac = dscore.y.y,
           match_jw = clean.y, dscore_jw = dscore)
  
  # ---- BEST MATCH ----
  ## function to extract modes
  modal <- function(x) {
    freq <- table(x) %>% as.data.frame(stringsAsFactors = FALSE)
    result <- freq[which(freq$Freq == max(freq$Freq)), 1]
    if (length(result) > 1){
      result <- str_c(result, collapse = "+")
    }
    result
  }
  
  ## extract modes
  match_dict_modes <- match_dict %>%
    mutate(match_dl = ifelse(is.na(match_dl), "0", match_dl),
           match_qgram = ifelse(is.na(match_qgram), "0", match_qgram),
           match_cos = ifelse(is.na(match_cos), "0", match_cos),
           match_jac = ifelse(is.na(match_jac), "0", match_jac),
           match_jw = ifelse(is.na(match_jw), "0", match_jw),
           dscore_dl = ifelse(is.na(dscore_dl), 0, dscore_dl),
           dscore_qgram = ifelse(is.na(dscore_qgram), 0, dscore_qgram),
           dscore_cos = ifelse(is.na(dscore_cos), 0, dscore_cos),
           dscore_jac = ifelse(is.na(dscore_jac), 0, dscore_jac),
           dscore_jw = ifelse(is.na(dscore_jw), 0, dscore_jw)) %>%
    select(ED, raw, clean, match_dl, match_qgram, 
           match_cos, match_jac, match_jw) %>%
    gather("method", "match", - c("ED", "raw", "clean")) %>%
    group_by(ED, raw) %>%
    summarise(mode = modal(match))
  
  ## join to match_dict
  match_dict <- left_join(match_dict, match_dict_modes, by = c("ED", "raw"))
  
  ## best match
  match_dict <- match_dict %>%
    mutate(match_dl = ifelse(is.na(match_dl), "0", match_dl),
           match_qgram = ifelse(is.na(match_qgram), "0", match_qgram),
           match_cos = ifelse(is.na(match_cos), "0", match_cos),
           match_jac = ifelse(is.na(match_jac), "0", match_jac),
           match_jw = ifelse(is.na(match_jw), "0", match_jw),
           dscore_dl = ifelse(is.na(dscore_dl), 0, dscore_dl),
           dscore_qgram = ifelse(is.na(dscore_qgram), 0, dscore_qgram),
           dscore_cos = ifelse(is.na(dscore_cos), 0, dscore_cos),
           dscore_jac = ifelse(is.na(dscore_jac), 0, dscore_jac),
           dscore_jw = ifelse(is.na(dscore_jw), 0, dscore_jw)) %>%
    rowwise() %>%
    mutate(result_type = ifelse(match_dl == match_cos & match_dl == match_qgram & match_dl == match_jac & match_dl == match_jw, ifelse(clean == match_dl, 1, ifelse(mode == 0, 6, 2)), ifelse(str_detect(mode, "\\+"), 4, ifelse(mode == 0, 5, 3)))) %>%
    mutate(best_match = case_when(result_type %in% c(1, 2, 3) ~ mode,
                                  result_type %in% c(4, 5, 6) ~ "0"))
  
  # ---- FILL DOWN ----
  ## create flags for row that had to be filled
  match_dict <- mutate(match_dict, flag = ifelse(best_match == 0, 1, 0))
  match_dict$index <- 1:nrow(match_dict)
  
  na_matches <- filter(match_dict, best_match == 0)
  
  ## fill down
  for (i in na_matches$index) {
    # extract pool of potential matches
    i <- as.numeric(i)
    match_ED <- na_matches[na_matches$index == i, "ED"]
    pool_indexes <- c(i - 3, i - 2, i - 1, i + 1, i + 2, i + 3)
    pool_matches <- filter(match_dict, index %in% pool_indexes & ED == match_ED) %>%
      select(best_match) %>%
      distinct()
    
    # match using jw
    match <- stringdist_left_join(filter(na_matches, index == i), pool_matches, by = c(clean = "best_match"), 
                                  max_dist = 5, method = "jw", distance_col = "dscore") %>%
      group_by(raw) %>%
      arrange(dscore) %>%
      slice(1)
    
    # sub match into match_dict
    match_dict[match_dict$index == i, "best_match"] <- match$best_match.y
  }
  
  # ---- MERGE TO ORIGINAL SAMPLE ----
  match_dict_subset <- select(match_dict, ED, raw, best_match, result_type, flag_st = flag)
  sample_cleaned <- left_join(rawData, match_dict_subset, by = c("ED" = "ED", "street_add" = "raw"))
}
