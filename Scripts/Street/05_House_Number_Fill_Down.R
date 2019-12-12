
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
  
  x <- df %>% group_by(ED, best_match) %>%
    fill(house_num, .direction="down") %>%
    ungroup()
  return(x)
}

#' Input to the function is an output from `04___`
sample_hn_filled <- fillDownHouseNum(`dataframe from 04 script here`)