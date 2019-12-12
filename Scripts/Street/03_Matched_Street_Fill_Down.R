
#' fillDownStreet
#' 
#' Fill down matched street names within the same enumeration page and ED.
#' This function is to be run on output from `02_Street_Matching_MNBK.R`.
#' @param df A dataframe with `microfilm`, `ED`, and `best_match` columns.
#' @return A dataframe with cleaned street filled down within an enumeration 
#' page and ED (group_by(`microfilm`, `ED`)).
fillDownStreet <- function(df){
  
  #' This line can be removed if df has only H record.
  df <- df %>% filter(record == "H")
  
  x <- df %>% group_by(microfilm, ED) %>%
    fill(best_match, .direction="down") %>%
    ungroup()
  return(x)
}

#' sample_cleaned is an output from `02_Street_Matching_MNBK.R`
sample_st_filled <- fillDownStreet(sample_cleaned)