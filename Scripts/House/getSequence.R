
## The keyword below is for Rmd render
## @knitr getSeq_fxs

#' getSequenceHead
#'
#' This is an internal function called by appendSeqCol(). It should never be used elsewhere
#' without a thorough review. It iterates over all house_nums enumeration forms to detect 
#' potential begining points of new subsequences. Indices of house_nums that are heads of 
#' subsequences are returned.
#' @param sample_hn_seq A string of house_num from enumeration pages. The string can (and should)
#' contain`NA`s if records have missing house_num.
#' @return A list of house_num indices that start new subsequences. The indices reference 
#' the param house_num string.
getSequenceHead <- function(sample_hn_seq){
  
  
  ###### internal helper functions start here #############
  
  
  #' Return (head_index, new_prev_index, new_current_index, new_next_index)
  #' `head_index` is added to a list of head indices
  #' The last three are used to find next head indices.
  isHead <- function(prev_index, curr_index, next_index){
    if (next_index>list_length){
      stop()
    }
    if (next_index>list_length){
      return() ## out-of-bound. stop.
    }
    
    if(is.na(next_index)) return()
    
    same_parity <- sameParity(getHouseNum(prev_index), getHouseNum(curr_index))
    # diff parity --> start new seq
    if(!same_parity)return(c(curr_index, curr_index, curr_index+1, curr_index+2))
    
    # same parity and same direction --> still in the seq
    if(sameDirection(prev_index, curr_index)) return(c(NA, curr_index, curr_index+1, curr_index+2))
    
    # same parity but diff direction
    dist <- checkDistance(prev_index, curr_index, next_index)
    if(dist >= 0){
      return(c(curr_index, curr_index, curr_index+1, curr_index+2))
    } else if(dist==-1){
      return(c(curr_index+1, curr_index+1, curr_index+2, curr_index+3))
    } else{
      message("Something is wring. dist = ", dist) # should never gets to here
      stop()
    }
  }
  
  #' Get a house number for a given house_num index
  getHouseNum <- function(i){
    return(nona_sample_hn_seq[i])
  }
  
  #' Get the moving direction of the current house_num
  #' Return 1 if the current is increasing from the prev house_num
  #' Return -11 if the current is decreasing from the prev house_num
  #' Return 0 if the current = the prev
  getDirections <- function(){
    dir_list <- c()
    dir <- diff(nona_sample_hn_seq)
    for(d in dir){
      if(d>0) current_dir <- 1
      else if(d<0) current_dir <- -1
      else current_dir <- 0
      
      dir_list <- c(dir_list, current_dir)
    }
    
    ## if current direction does not change, take previous direction as current
    for(i in seq(2, length(dir_list))){
      if(dir_list[i]==0) dir_list[i] <- dir_list[i-1] 
    }
    return(dir_list)
  }
  
  #' Check if the current and the previous house_nums are both decreasing (increasing).
  #' Return TRUE if they are. FALSE otherwise. 
  sameDirection <- function(prev_index, curr_index){
    if(dir_list[prev_index]==dir_list[curr_index]) return(TRUE)
    else return(FALSE)
  }
  
  #' Check the distance from the current house_num to the previous house_num
  #' and the distance from the current to the next house_num. Return 1 if the
  #' former is smaller, -1 if the latter is smaller, and 0 if they equal.
  checkDistance <- function(prev_index, curr_index, next_index){
    prev_dist <- abs(getHouseNum(prev_index) - getHouseNum(curr_index))
    next_dist <- abs(getHouseNum(next_index) - getHouseNum(curr_index))
    if(prev_dist<next_dist) return(-1) # go with the prev seq
    else if (prev_dist>next_dist) return(1) # go with the next seq
    else return(0) # equal distance -> vague
  }
  
  ## Return TRUE if num_1 and num_2 have the same parity
  sameParity <- function(num_1, num_2){
    if (isEven(num_1+num_2))return(TRUE)
    else return(FALSE)
  }
  
  ## Return TRUE if num_1 is even
  isEven <- function(num_1){
    if(num_1%%2==0) return(TRUE)
    else return(FALSE)
  }
  
  
  ###### internal helper functions end here #############
  
  
  ## Get indices of param with non-NA
  not_na_index_ref_raw <- which(!is.na(sample_hn_seq))
  
  ## Get house_nums (non-NA)
  nona_sample_hn_seq <- sample_hn_seq[not_na_index_ref_raw]
  
  ## Assume the first non-NA house_num is a head of the first subsequence
  current_index_of_heads <- c(1)
  
  ## Get dirrection list
  dir_list <- getDirections()
  
  list_length <- length(nona_sample_hn_seq)
  
  #' This while loop iterates over all house_num in the string.
  #' New head index is appended to current_index_of_heads
  #' New set of house_num indices are returned for further iteration 
  #' until it reaches the end of the house_num string.
  prev_i <- 1
  curr_i <- 2
  next_i <- 3
  while(next_i <= list_length){
    result <- isHead(prev_i,curr_i,next_i)
    if (!is.na(result[1])){
      current_index_of_heads <- c(current_index_of_heads, result[1])
    }
    prev_i <- result[2]
    curr_i <- result[3]
    next_i <- result[4]
  }
  index_of_seq_ref_raw <- not_na_index_ref_raw[current_index_of_heads]
  return(index_of_seq_ref_raw)
}

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
appendSeqCol <- function(sample_df){
  
  ## Setup
  sample_df <- sample_df %>% mutate(house_num = as.numeric(house_num))
  x <- sample_df$house_num
  
  ## Get index of house_num that starts a new subsequence (sequence head)
  begin <- getSequenceHead(x)
  
  ## Create a new vector (for a column).
  SEQ <- rep(NA, length(x))
  
  ## Assign unique seq number to house_nums that start new subsequences
  SEQ[begin] <- seq(1,length(begin))
  sample_df["SEQ"] <- SEQ
  
  ## Determine subsequence parities
  ## Fill down SEQ and seq_par for records under subsequence head
  r <- sample_df %>% 
    mutate(seq_par = ifelse(is.na(house_num), NA, ifelse(house_num%%2==0, 1, 0))) %>% 
    tidyr::fill(seq_par, .direction = "down") %>%
    tidyr::fill(SEQ, .direction = "down") 
  return(r)
}


