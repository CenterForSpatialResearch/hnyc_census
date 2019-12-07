
## The code below is for Rmd rendering
## @knitr  read_merge_fxs

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
mergeSeq <- function(sdf, check_street = FALSE){
  
  if((sdf %>% pull(SEQ) %>% unique() %>% length) != 2){
    message("must provide 2 sequences:",sdf %>% pull(SEQ) %>% unique() %>% length)
    stop()
  }
  par <- chkPar(sdf)
  jump <- chkJump(sdf, 10)
  
  #' Check street if turned on
  if(check_street) str <- chkStreet(sdf)
  else str <- TRUE
  
  if (par && jump && str) return(TRUE)
  else return(FALSE)
}

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
#' @param p1 A dataframe with `street_add` column.
#' @return TRUE if all streets in `p1` have the exact same name. Otherwise, returns FALSE.
chkStreet <- function(p1){
  if(p1$street_add %>% unique() %>% na.omit %>% length != 1) return(FALSE)
  else return(TRUE)
}

#' chkJump
#'
#' This is an internal function, called by mergeSeq(). It checks whether house_nums in `p`
#' skips with a size smaller than a certain jump size.
#' @param p1 A dataframe with `house_num` column.
#' @return TRUE if the skips between consecutive house_nums in `p` are not greater than 
#' `jump_size`. Otherwise, return `FALSE`.
chkJump <- function(p1, jump_size){
  x <- p1 %>% filter(!is.na(house_num)) %>% pull(house_num)
  for(i in seq(1, length(x)-1)){
    if(abs(x[i] - x[i+1]) > jump_size) return(FALSE)
  }
  return(TRUE)
}

#' appendMergeSeq
#'
#' This function returns a dataframe with a new column of merged sequence numbers (`merge_SEQ`). 
#' @param sdf A dataframe to be appended with house number sequence column. It is recommended
#' that the dataframe is an output from `appendSeqCol()`.
#' @return A dataframe with appended merged sequence numbers (`merge_SEQ`).
#' @export
#' @examples
#' HN7 <- appendMergeSeq(HN6) 
appendMergeSeq <- function(sdf){
  
  #' Convert SEQ into numeric
  sdf <- sdf %>% mutate(SEQ = as.numeric(as.character(SEQ)))
  
  #' Get the list of non-NA house_num sequence
  seq_list <- sdf %>% pull(SEQ) %>% unique() %>% na.omit
  seq_list <- seq_list[which(!is.na(seq_list))]
  
  #' Split `sdf` into small dataframes by their sequence numbers
  df_by_SEQ <- split( sdf , f = sdf$SEQ )
  
  #' Calls mergeSeq() to check if SEQ i can be mergeg into SEQ i-1
  mergeable <- c()
  for(i in seq(1, length(seq_list)-1)){
    mergeable[i]<-mergeSeq(rbind(df_by_SEQ[[i]], df_by_SEQ[[i+1]]))
  }
  
  appended_df <- sdf %>% mutate(merge_SEQ = SEQ)
  
  #' Set `merge_SEQ` of `SEQ` that will be merged to previous subsequence as NA
  #' Then, fill down `SEQ`.
  meageable_seq <- which(mergeable==TRUE) + 1
  appended_df <- appended_df %>%
    mutate(merge_SEQ = ifelse(SEQ %in% meageable_seq, NA, merge_SEQ)) %>% 
    fill(merge_SEQ, .direction = "down") %>%
    mutate(merge_SEQ = as.factor(merge_SEQ),
           SEQ = as.factor(SEQ))
  return(appended_df)
}


## @knitr  create_HN7

## sample code
# HN6 <- appendSeqCol(HN5, jump_size = 500)
# HN7 <- appendMergeSeq(HN6)

#save(HN7, file="HN7.RData")


