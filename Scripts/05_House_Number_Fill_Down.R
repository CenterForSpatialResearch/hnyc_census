
#' fillDownHouseNum
#' 
#' Fill down house number within the same ED and matched street name.
#' This function is to be run on output from `04_____`.
#' @param df A dataframe with `best_match` and `house_num` columns. `house_num` 
#' column must be cleaned. 
#' @return A dataframe with filled down house numbers (group_by(`ED`, `best_match`))
#' and `flg_filled_hn` column. The value of `flg_filled_hn` is 1 if `house_num` of
#' a record is filled down. 0 otherwise. 
fillDownHouseNum <- function(df){
  
  df <- df %>% 
    filter(record == "H") %>%
    mutate(house_num_temp = house_num) %>% 
    group_by(ED, best_match) %>%
    fill(modifier.number, modifier.word, house_num, hn_1, hn_2, hn_3, .direction="down") %>%
    fill(modifier.number, modifier.word, house_num, hn_1, hn_2, hn_3, .direction="up") %>%
    rowwise() %>% 
    mutate(flg_filled_hn = ifelse(!is.na(house_num)  && is.na(house_num_temp), 1, 0)) %>% 
    select(- house_num_temp) %>%
    ungroup()
  return(df)
}

#` ############ IMPORTANT !! change input to this function call to output from 04_.R
#sample_hn_filled <- fillDownHouseNum(sample_st_filled)

#' One test for fillDownHouseNum(). Should not get error message from
#' running this line after if the function works properly. This checks
#' if house_num is not filled down but the flag says it is.
#assertthat::assert_that(nrow(sample_hn_filled %>% filter(is.na(house_num) && flg_filled_hn==1)) == 0)

