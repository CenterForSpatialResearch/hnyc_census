
#' fillDownStreet
#' 
#' Fill down matched street names within the same enumeration page and ED.
#' This function is to be run on output from `02_Street_Matching_MNBK.R`.
#' @param df A dataframe with `microfilm`, `ED`, and `best_match` columns.
#' @return A dataframe with cleaned street filled down (in the same column
#'  `best_match`) within an enumeration page and ED 
#'  (group_by(`microfilm`, `ED`)). A new flag column (`flg_filled_st`) is 
#'  also appended. The value is 1 if a `best_match` in that record is 
#'  filled down.
fillDownStreet <- function(df){
  
  df <- df %>%
    filter(record == "H") %>%
    mutate(best_match_temp = best_match) %>% 
    group_by(ED) %>%
    fill(street_add, best_match, result_type, .direction="down") %>%
    fill(street_add, best_match, result_type, .direction = "up") %>%
    rowwise() %>% 
    mutate(flg_filled_st = ifelse(!is.na(best_match)  && is.na(best_match_temp), 1, 0)) %>% 
    select(-best_match_temp) %>% 
    ungroup()
  
  return(df)
}

#' sample_cleaned is an output from `02_Street_Matching_MNBK.R`
# sample_st_filled <- fillDownStreet(sample_cleaned)

#' One test for fillDownStreet(). Should not get error message from
#' running this line after if the function works properly. This checks
#' if best_match is not filled down but the flag says it is.
# assertthat::assert_that(nrow(sample_st_filled %>% filter(is.na(best_match) && flg_filled_st==1)) == 0)
