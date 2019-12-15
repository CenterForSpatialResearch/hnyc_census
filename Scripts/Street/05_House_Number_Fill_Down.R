
#' fillDownHouseNum
#' 
#' Fill down house number within the same ED and matched street name.
#' This function is to be run on output from `04_____`.
#' @param df A dataframe with `best_match` and `house_num` columns. `house_num` 
#' column must be cleaned. 
#' @return A dataframe with filled down house numbers (group_by(`ED`, `best_match`)).
fillDownHouseNum <- function(df){
  
  #' This line can be removed if df has only H record.
  df <- df %>% filter(record == "H")
  
  #' Fill down house_num and create a flag if record's house num is filled.
  x <- df %>% mutate(house_num_temp = house_num) %>% 
    group_by(ED, best_match) %>%
    fill(house_num, .direction="down") %>%
    rowwise() %>% 
    mutate(flg_filled_hn = ifelse(!is.na(house_num)  && is.na(house_num_temp), 1, 0)) %>% 
    #select(-best_match_temp) %>% 
    ungroup()
  return(x)
}

#' Input to the function is an output from `04___`
sample_hn_filled <- fillDownHouseNum(sample_st_filled)

#' One test for fillDownHouseNum(). Should not get error message from
#' running this line after if the function works properly. This checks
#' if house_num is not filled down but the flag says it is.
assertthat::assert_that(nrow(sample_hn_filled %>% filter(is.na(house_num) && flg_filled_hn==1)) == 0)

