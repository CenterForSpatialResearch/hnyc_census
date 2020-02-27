
## The keyword below is for Rmd render
## @knitr getSeq_fxs

# ---- Helper function: getSequenceHead ----
#' getSequenceHead
#'
#' This is an internal function called by appendSeqCol(). It iterates over all house_nums enumeration forms to detect 
#' potential begining points of new subsequences. Indices of house_nums that are heads of 
#' subsequences are returned.
#' @param sample_hn_seq A string of house_num from enumeration pages. The string can (and should)
#' contain`NA`s if records have missing house_num.
#' @return A vector of indices c(seq_head_index, prev_index, current_index, next_index).
getSequenceHead <- function(sample_hn_seq, jump_size, check_parity = TRUE, check_dir = TRUE){
  
  ## Get indices of param with non-NA
  not_na_index_ref_raw <- which(!is.na(sample_hn_seq))
  
  ## Get house_nums (non-NA)
  nona_sample_hn_seq <- sample_hn_seq[not_na_index_ref_raw]
  
  ## Assume the first non-NA house_num is a head of the first subsequence
  heads <- c(1)
  
  ## Get direction list
  dir_list <- getDirectionalHeads(nona_sample_hn_seq)
  
  list_length <- length(nona_sample_hn_seq)
  
  #' This while loop iterates over all house_num in the string.
  #' New head index is appended to current_index_of_heads
  #' New set of house_num indices are returned for further iteration 
  #' until it reaches the end of the house_num string.
  prev_i <- 1
  curr_i <- 2

  while(curr_i <= list_length){
    
    isCurrentIndexASeqHead <- isHead(prev_i, curr_i, dir_list, nona_sample_hn_seq, check_parity = check_parity, check_dir = check_dir, jump_size = jump_size)
    if (isCurrentIndexASeqHead) {
      heads <- append(heads, curr_i)
    }
    
    prev_i <- curr_i
    curr_i <- curr_i + 1
    
  }
  
  return(heads)
}


#---- Helper Functions for getSequenceHead: isHead ----
#' isHead
#' helper function for getSequenceHead
#' Return (head_index, new_prev_index, new_current_index, new_next_index)
#' `head_index` is added to a list of head indices
#' The last three are used to find next head indices.
isHead <- function(prev_index, curr_index, dir_list, hn_seq, check_parity = TRUE, check_dir = TRUE, jump_size){
  if (curr_index > nrow(dir_list)){
    warning(paste0('Index is out of bounds of direction lists. Current index: ', curr_index))
    return() ## out-of-bound. stop.
  }
  
  if(is.na(curr_index)) return()
  
  prev_hn <- getHouseNum(prev_index, hn_seq)
  curr_hn <- getHouseNum(curr_index, hn_seq)
  
  count_false <- !withinJumpSize(prev_hn, curr_hn, jump_size)
  
  if (check_parity) {
    count_false <- count_false + !sameParity(prev_hn, curr_hn)
  }
  
  if (check_dir) {
    count_false <- count_false + isDirectionalHead(curr_index, dir_list)
  }
  
  return(count_false > 0)
  
}

# ---- Helper Functions for isHead ----
#' getHouseNum
#' Get a house number for a given house_num index
getHouseNum <- function(index, hn_seq){
  return(hn_seq[index])
}

#' getDirections
#' Get the moving direction of the current house_num
#' Return 1 if the current is increasing from the prev house_num
#' Return -11 if the current is decreasing from the prev house_num
#' Return 0 if the current = the prev
#' returns a vector of directions 
getDirectionalHeads <- function(seq){

  diff <- diff(seq)
  dir_list <- tibble(actual = c(0, diff),
                     isHead = rep(FALSE, length(seq)),
                     seq = rep(0, length(seq)))
  
  dir_list <- dir_list %>%
    mutate(dir = case_when(actual == 0 ~ 0,
                           actual > 0 ~ 1,
                           actual < 0 ~ -1))
  dir_list$isHead[1] <- TRUE
  dir_list$seq[1] <- 1
  
  for (i in seq(2, nrow(dir_list))) {
    if (dir_list$dir[i] == 0) { # if house num is same as previous house num
      dir_list$seq[i] <- dir_list$seq[i-1] # isHead still FALSE, seq = prev seq
      dir_list$dir[i] <- dir_list$dir[i-1] # ensure this inherits the direction of seq
      
    } else if (dir_list$dir[i] == dir_list$dir[i-1]) { # if dir is same as prev dir
      dir_list$seq[i] <- dir_list$seq[i-1] # inherit seq no.
      
    } else { # direction is not same as previous after inheriting seq dir
      
      if (dir_list$dir[i-1] == 0) { # but previous dir = 0, i.e. everything before was same no and this is the same seq. isHead is FALSE, seq = prev seq.
        dir_list$seq[i] <- dir_list$seq[i-1]
        
        #  BUT update sequence's dir to current dir.
        dir_list <- dir_list %>%
          mutate(dir = ifelse(seq == dir_list$seq[i], dir_list$dir[i], dir))
      } else {
        dir_list$isHead[i] <- TRUE
        dir_list$seq[i] <- dir_list$seq[i-1] + 1
        dir_list$dir[i] <- 0
      }
      
    }
  }
  
  return(dir_list)
  
}

#' sameDirection
#' Check if the current and the previous house_nums are both decreasing (increasing).
#' Return TRUE if they are. FALSE otherwise. 

isDirectionalHead <- function(curr_index, dir_list){
  return(dir_list$isHead[[curr_index]])
}

#' sameParity
#' Return TRUE if num_1 and num_2 have the same parity
sameParity <- function(num_1, num_2){
  
  isEven <- function(num_1){
    if(num_1%%2==0) return(TRUE)
    else return(FALSE)
  }
  
  if (isEven(num_1+num_2))return(TRUE)
  else return(FALSE)
  
}

#' withinJumpSize
withinJumpSize <- function(prev_hn, curr_hn, jump_size){
  if(abs(prev_hn - curr_hn) > jump_size) return(FALSE)
  else return(TRUE)
}

#---- Main Function: appendSeqCol ----
#' appendSeqCol
#'
#' This function returns a dataframe with new columns of subsequence numbers (`SEQ`), 
#' unique record id (`i`), and house number parity of subsequences (`seq_par`).
#' @param sample_df A dataframe to be appended with house number sequence column
#' @return A dataframe with appended house number sequence (`SEQ`), unique record id (`i`), 
#' and house number parity of subsequences (`seq_par`) columns.
#' @export
#' @examples
#' HN6 <- appendSeqCol(HN5)
appendSeqCol <- function(df, check_street = TRUE, check_parity = TRUE, check_dir = TRUE, jump_size){
  
  ## Setup
  df$index <- 1:nrow(df)
  sample_df <- df %>% filter(!is.na(hn_1))
  sample_df <- sample_df %>% mutate(hn_1 = as.numeric(hn_1)) #! uses house_num, which is not cleaned. change to hn_1
  x <- sample_df$hn_1
  
  ## Get index of house_num that starts a new subsequence (sequence head)
  begin <- getSequenceHead(x, check_parity = check_parity, check_dir = check_dir, jump_size)
  
  ## Create a new vector (for a column).
  SEQ <- rep(NA, length(x))
  
  ## Assign unique seq number to house_nums that start new subsequences
  SEQ[begin] <- seq(1,length(begin))
  sample_df["SEQ"] <- SEQ
  
  ## Determine subsequence parities
  ## Fill down SEQ and seq_par for records under subsequence head
  r <- sample_df %>% 
    mutate(seq_par = ifelse(is.na(hn_1), NA, ifelse(hn_1%%2==0, 1, 0))) %>% 
    tidyr::fill(seq_par, .direction = "down") %>%
    tidyr::fill(SEQ, .direction = "down") 
  
  #' Also consider street names if check_street is TRUE
  if(check_street){
    r <- getSeqByStreet(r)
  }
  r <- select(r, index, SEQ, seq_par)
  r <- left_join(df, r, by = c("index" = "index"))
  r <- r %>%
    fill(SEQ, .direction = "down") %>%
    fill(SEQ, .direction = "up")
  
  return(r)
}


#' getSeqByStreet
#'
#' This is an internal function, called by appendSeqCol when street names need to be checked
#' during seq detection. It determines a joint sequences using both seq by house_nums and 
#' seq by street names. The new joint sequence column is created in place of the old `SEQ`
#' column.
#' @param df A dataframe with `SEQ` that is determined by house_num string.
#' @return A dataframe with the new `SEQ` column.
getSeqByStreet <- function(df){
  
  #' Check type of Stree_add column
  df <- df %>% mutate(best_match = as.character(best_match),
                      best_match = ifelse(is.na(best_match), NA, 
                                          ifelse(best_match=="", NA,best_match)))
  
  current_street_i <- 1
  street_add_list <- df %>% pull(best_match)
  seq_by_street <- c(1, rep(NA, nrow(df) -1))
  
  #' Compare previous and current street names.
  #' Mark a new sequence if their names are different
  for(i in seq(2, length(street_add_list))){
    prev_st <- street_add_list[i-1]
    curr_st <- street_add_list[i]
    
    if(is.na(prev_st) && (!is.na(curr_st))){
      current_street_i = current_street_i + 1
    } else if ((!is.na(prev_st)) && is.na(curr_st)){
      current_street_i = current_street_i + 1
    } else if (is.na(prev_st) && is.na(curr_st)) {}#do nothing
    else if (prev_st != curr_st) {current_street_i = current_street_i + 1}
    seq_by_street[i] <- current_street_i
  }
  
  #' Append new seq by street to df
  df["SEQ_by_st"] <- seq_by_street
  
  #' Determine the new seq usingboth seq by house_num and seq by street name
  df2 <- df %>% mutate(new_seq = ifelse(is.na(SEQ), NA, paste(SEQ, SEQ_by_st, sep = "-")))
  new_seq <- df2 %>% pull(new_seq) %>% unique() %>% na.omit()
  seq_dict <- seq(1, length(new_seq))
  names(seq_dict) <- new_seq
  
  #' Append new seq to df. Replace old SEQ column with this new column
  df2 <- df2 %>% mutate(new_seq = as.character(new_seq),
                        SEQ = ifelse(is.na(new_seq), NA, seq_dict[new_seq])) %>%
    select(-SEQ_by_st, -new_seq)
  
  return(df2)
}


