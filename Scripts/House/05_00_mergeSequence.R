
## The code below is for Rmd rendering
## @knitr  read_merge_fxs


#---- mergeSeq() ----
#' mergeSeq
#'
#' This is an internal function, called by appendMergeSeq(). It checks whether existing 
#' subsequences of a dataframe can be merged into 1 sequence by checking consitions on 
#' parity, jump size, and street name (optional).
#' @param sdf A dataframe with exactly 2 unique `SEQ`. 
#' @param check_street A boolean whether the function should check street name condition.
#' This could produce unreliable merged sequences if street names are inaccurate. The 
#' default is FALSE.
#' @return TRUE if 2 unique `SEQ` can be merged into 1 merged sequence.
mergeSeq <- function(sdf, jump_size = c(2, 30, 50), check_street = TRUE, check_parity = TRUE){
  
  if((sdf %>% pull(SEQ) %>% unique() %>% length) != 2){
    message("must provide 2 sequences:",sdf %>% pull(SEQ) %>% unique() %>% length)
    stop()
  }
  jump <- chkJump(sdf, jump_size)
  
  if (check_parity) par <- chkPar(sdf)
  else par <- TRUE
  
  #' Check street if turned on
  if(check_street) str <- chkStreet(sdf)
  else str <- TRUE
  
  if (par && jump && str) return(TRUE)
  else return(FALSE)
}

# ---- mergeSeq Helper functions ----
#' chkPar
#'
#' This is an internal function, called by mergeSeq(). It checks whether the number of 
#' house_nums with the minority parity does not exceed 2. For example, if `p1` contains 
#' house_nums 2, 4, 6, 7, 9, 10. There are 2 house_nums with odd parity and 4 house_nums 
#' with even parity. In this case, the function returns TRUE because 2 does not exceed 2.
#' @param p1 A dataframe with `seq_par` column.
#' @return TRUE if there are not more than 2 house_nums with different parity. 
#' Otherwise, returns FALSE.
chkPar <- function(p1){
  prop <- table(p1$seq_par)[1]/sum(table(p1$seq_par))
  # if (length(table(p1$seq_par)) == 1) return(TRUE)
  # else if (min(prop, 1- prop) < 0.1) return(TRUE)
  # else return(FALSE)
  if(length(table(p1$seq_par)) == 1) return(TRUE)
  else if (min(table(p1$seq_par)[1], table(p1$seq_par)[2]) > 2) return(TRUE)
  else return(FALSE)
}

#' chkStreet
#'
#' This is an internal function, called by mergeSeq(). It checks whether street in `p1`
#' have the same names. 
#' @param p1 A dataframe with `best_match` column.
#' @return TRUE if all streets in `p1` have the exact same name. Otherwise, returns FALSE.
chkStreet <- function(p1){
  #' ignore NA. Missing house numbers will not break sequences
  if(p1$best_match %>% unique() %>% na.omit %>% length != 1) return(FALSE)
  else return(TRUE)
}

#' chkJump
#'
#' This is an internal function, called by mergeSeq(). It checks whether house_nums in `p`
#' skips with a size smaller than a certain jump size.
#' @param p1 A dataframe with `hn_1` column.
#' @return TRUE if the skips between consecutive house_nums in `p` are not greater than 
#' `jump_size`. Otherwise, return `FALSE`.
chkJump <- function(p1, jump_size){
  x <- p1 %>% filter(!is.na(hn_1)) %>% pull(hn_1) # change house_num to hn_1
  
  if (length(jump_size) != 3) {
    warning("Jump size should be length 3")
  }
  
  #' There can be only 1 or no house_num
  if (length(x)<2) return(TRUE)
  
  for(i in seq(1, length(x)-1)){ 
    # if two house numbers are of different lengths, use the jump size of the smaller house number
    curr_str_length = str_length(x[i])
    next_str_length = str_length(x[i+1])
    
    if (curr_str_length > next_str_length) {
      ind = next_str_length
    } else {
      ind = curr_str_length
    }
    
    if (ind > length(jump_size)) {ind = length(jump_size)}

    if(abs(x[i] - x[i+1]) > jump_size[ind]) return(FALSE)
  }
  return(TRUE)
}

#---- appendMergeSeq----
#' appendMergeSeq
#'
#' This function returns a dataframe with a new column of merged sequence numbers (`merge_SEQ`). 
#' @param sdf A dataframe to be appended with house number sequence column. It is recommended
#' that the dataframe is an output from `appendSeqCol()`.
#' @return A dataframe with appended merged sequence numbers (`merge_SEQ`).
#' @export
#' @examples
#' HN7 <- appendMergeSeq(HN6) 
appendMergeSeq <- function(sdf, jump_size = c(2, 30, 50), check_street = TRUE, check_parity = TRUE){
  
  #' Convert SEQ into numeric
  sdf <- sdf %>% mutate(SEQ = as.numeric(as.character(SEQ)))
  
  #' Get the list of non-NA house_num sequence
  seq_list <- sdf %>% pull(SEQ) %>% unique() %>% na.omit
  seq_list <- seq_list[which(!is.na(seq_list))]
  
  #' Split `sdf` into small dataframes by their sequence numbers
  df_by_SEQ <- split(sdf , f = sdf$SEQ )
  
  #' Calls mergeSeq() to check if SEQ i can be mergeg into SEQ i-1
  mergeable <- c()
  for(i in seq(1, length(seq_list)-1)){ # loops through each seq
    mergeable[i]<-mergeSeq(rbind(df_by_SEQ[[i]], df_by_SEQ[[i+1]]), jump_size = jump_size, check_street = check_street, check_parity = check_parity)
  }
  
  appended_df <- sdf %>% mutate(merge_SEQ = SEQ)
  
  #' Set `merge_SEQ` of `SEQ` that will be merged to previous subsequence as NA
  #' Then, fill down `SEQ`.
  meageable_seq <- seq_list[which(mergeable==TRUE) + 1]
  appended_df <- appended_df %>%
    mutate(merge_SEQ = ifelse(SEQ %in% meageable_seq, NA, merge_SEQ)) %>% 
    fill(merge_SEQ, .direction = "down") %>%
    mutate(merge_SEQ = as.factor(merge_SEQ),
           SEQ = as.factor(SEQ))
  return(appended_df)
}

# ---- getMergeSeq----
#' Wrap two step merging (subsequence and merged sequence) into 1 function
#' It will be more efficicent to internalize subsequencne step into merged
#' sequence step.
#' @param df A dataframe with `house_num`, `best_match`, and `i` columns
#' @param get_jump_size Used in the subsequence step. If house_num jumps > jump_size, 
#' break into subsequences.
#' @param get_check_parity Used in the subsequence step. Should sequence generation check parity of house numbers?
#' @param get_check_dir Used in the subsequence step. Should sequence generation check the direction (increasing/decreasing) of house numbers?
#' @param get_check_street Used in the subsequence step. Should sequence generation check that all streets in the sequence are the same?
#' @param get_jump_size Used in the subsequence step. Largest difference in house numbers in sequence generation. This should be a raw number, e.g. 10
#' @param merge_jump_size Used in merging step. Should be a vector of jump sizes for each digit length, in the format of a vector, c(max_jump_2digithn, max_jump_3digithn, max_jump_4ormoredigithn). See '05__jump_size_EDA.Rmd' for how defaults were derived. 
#' @param merge_check_street Used in merging step. Should merged sequences still have the same street name?
#' @param merge_check_parity Used in merging step. Should merged sequences still have the same parity?
#' @return A dataframe with `SEQ` and `merge_SEQ` columns attached.
#' @export
#' @example 
#' HN7_hn <- getMergedSeq(HN5, jump_size = 500, check_street = FALSE)
#' HN7_st <- getMergedSeq(HN5, jump_size = 500, check_street = TRUE)
getMergedSeq <- function(df, get_check_street = TRUE, get_check_parity = TRUE, get_check_dir = T, get_jump_size = 10, merge_jump_size = c(2, 30, 50), merge_check_street = T, merge_check_par = T){
  temp <- appendSeqCol(df, check_street = get_check_street, check_parity = get_check_parity, check_dir = get_check_dir, jump_size = get_jump_size)
  HN7 <- appendMergeSeq(temp, jump_size = merge_jump_size, check_street = merge_check_street, check_parity = merge_check_par)
  return(HN7)
}

## #' Example
# HN7_hn <- getMergedSeq(HN5, jump_size = 500, check_street = FALSE)
# HN7_st <- getMergedSeq(HN5, jump_size = 500, check_street = TRUE)

#save(HN7, file="HN7.RData")

