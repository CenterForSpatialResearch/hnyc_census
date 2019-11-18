trampoline <- function(f, ...) {
  function(...) {
    ret <- f(...)
    while (inherits(ret, "recursion")) {
      ret <- eval(as.call(c(f, unclass(ret))))
    }
    ret
  }
}

recur <- function(...) {
  structure(c(...), class = "recursion")
}

# called by appendSeqCol()
getSequenceHead <- function(sample_hn_seq){
  
  ## use index referencing nona_sample_hn_seq (consecutive)
  ## must start from the second HN
  ## return (head_index, new_prev_index, new_current_index, new_next_index)
  isHead <- function(prev_index, curr_index, next_index){
    message(next_index,"\n")
    if (next_index>list_length){
      message("must stop")
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
      message("Something is wring. dist = ", dist)
      stop()
    }
  }
  
  ## get the actual house number for a given index
  getHouseNum <- function(i){
    return(nona_sample_hn_seq[i])
  }
  
  ## use the same index as nona_sample_hn_seq
  ## return 1 if i is going up relative to i-1
  ## return -1 if i is going down relative to i-1
  ## return 0 if i = i-1
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
  
  sameDirection <- function(prev_index, curr_index){
    if(dir_list[prev_index]==dir_list[curr_index]) return(TRUE)
    else return(FALSE)
  }
  
  checkDistance <- function(prev_index, curr_index, next_index){
    prev_dist <- abs(getHouseNum(prev_index) - getHouseNum(curr_index))
    next_dist <- abs(getHouseNum(next_index) - getHouseNum(curr_index))
    if(prev_dist<next_dist) return(-1) # go with the prev seq
    else if (prev_dist>next_dist) return(1) # go with the next seq
    else return(0) # equal distance -> vague
  }
  
  ## return TRUE if num_1 and num_2 have the same parity
  sameParity <- function(num_1, num_2){
    if (isEven(num_1+num_2))return(TRUE)
    else return(FALSE)
  }
  
  ## return TRUE if num_1 is even
  isEven <- function(num_1){
    if(num_1%%2==0) return(TRUE)
    else return(FALSE)
  }
  
  
  ## index of non NA in raw sequence
  not_na_index_ref_raw <- which(!is.na(sample_hn_seq))
  
  nona_sample_hn_seq <- sample_hn_seq[not_na_index_ref_raw]
  
  
  ## current list of indices of heads
  current_index_of_heads <- c(1)
  
  ## dirrection list
  dir_list <- getDirections()
  
  list_length <- length(nona_sample_hn_seq)
  
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

appendSeqCol <- function(sample_df){
  x <- sample_df$house_num %>% as.numeric()
  begin <- getSequenceHead(x)
  SEQ <- rep(NA, length(x))
  SEQ[begin] <- seq(1,length(begin))
  sample_df["SEQ"] <- SEQ
  return(sample_df %>% fill(SEQ, .direction = "down"))
}

HN6 <- appendSeqCol(HN5)
class(HN5$house_num)

