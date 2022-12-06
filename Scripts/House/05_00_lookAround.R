
## @knitr  lookAround

#' lookAround
#'
#' The function returns a subset of a dataframe. A subset contains row `i`-`window_size`/2 to row `i`+`window_size`/2.
#' @param df A dataframe to be subset. It must contain colum `i`.
#' @param ind An index of a dataframe. This is the middle of the window.
#' @param window_size An integer specifying how many rows around the record should 
#' be inspected. This is just a rough number.
#' @return A subset dataframe of size `window_size`.
lookAround <- function(df, ind, window_size){
  half <- window_size/2
  return(df %>% filter(i %in% seq(ind-half, ind+half)))
}

#' breakPointI
#'
#' The function is for inspecting rows around a record where only one of `SEQ` and 
#' `merge_SEQ` break. 
#' @param t A dataframe with `SEQ`, `merge_SEQ`, and `i` columns.
#' @return A vector of `i` of records at which only one of the two sequence types breaks.
breakPointI <- function(t){
  
  #' SEQ and merge_SEQ cannot be factors
  t <- t %>%
    mutate(SEQ = as.numeric(as.factor(SEQ)),
           merge_SEQ = as.numeric(as.factor(merge_SEQ)),
           SEQ = ifelse(is.na(SEQ), -1, SEQ),
           merge_SEQ = ifelse(is.na(merge_SEQ), -1, merge_SEQ))
  
  old_s <- t$SEQ
  new_s <- t$merge_SEQ
  
  #' A loop to detect different break points
  index_diff_seq <- c()
  for(ind in seq(1, length(old_s)-1)){
    if(is.na(old_s[ind]) || is.na(new_s[ind])) next()
    
    brk_old_seq <- FALSE
    brk_new_seq <- FALSE
    if(old_s[ind] != old_s[ind+1]) brk_old_seq <- TRUE
    if(new_s[ind] != new_s[ind+1]) brk_new_seq <- TRUE
    
    if(brk_old_seq!=brk_new_seq) index_diff_seq <- c(index_diff_seq, ind+1)
  }
  
  return(t[,"i"][index_diff_seq,] %>% pull(i))
  
}

