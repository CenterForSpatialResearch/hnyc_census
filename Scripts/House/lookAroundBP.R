
#' lookAroundBP
#'
#' The function is for inspecting rows around a record where only one of `SEQ` and 
#' `merge_SEQ` break. 
#' @param t A dataframe with `SEQ`, `merge_SEQ`, and `i` columns.
#' @param window_size An integer specifying how many rows around the record should 
#' be inspected. This is just a rough number.
#' @return A list of dataframes, each of which is with `window_size` rows around 
#' the record of inspection.
lookAroundBP <- function(t, window_size){
  
  t <- t %>%
    mutate(SEQ = as.numeric(as.factor(SEQ)),
           merge_SEQ = as.numeric(as.factor(merge_SEQ)),
           SEQ = ifelse(is.na(SEQ), -1, SEQ),
           merge_SEQ = ifelse(is.na(merge_SEQ), -1, merge_SEQ))
  
  old_s <- t$SEQ
  new_s <- t$merge_SEQ
  window_size <- window_size/2
  
  index_diff_seq <- c()
  for(i in seq(1, length(old_s)-1)){
    if(is.na(old_s[i]) || is.na(new_s[i])) next()
    
    brk_old_seq <- FALSE
    brk_new_seq <- FALSE
    if(old_s[i] != old_s[i+1]) brk_old_seq <- TRUE
    if(new_s[i] != new_s[i+1]) brk_new_seq <- TRUE
    
    if(brk_old_seq!=brk_new_seq) index_diff_seq <- c(index_diff_seq, i+1)
  }
  row_diff_seq <- t$i[index_diff_seq]
  
  diff_seq_list <- lapply(row_diff_seq, function(ind){
    return(HN7_hn %>% filter(i %in% seq(ind-window_size, ind+window_size)))
  })
  
  return(diff_seq_list)
}

x <- lookAroundBP(HN7_hn, window_size = 6)
