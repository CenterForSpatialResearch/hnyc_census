## merge sequences

## @knitr  read_merge_fxs

## return TRUE if 2 sequences in sdf can be merged into 1
## HAVE NOT TURN STREET CHECK ON
mergeSeq <- function(sdf){
  
  if((sdf %>% pull(SEQ) %>% unique() %>% length) != 2){
    message("must provide 2 sequences:",sdf %>% pull(SEQ) %>% unique() %>% length)
    stop()
  }
  par <- chkPar(sdf)
  str <- chkStreet(sdf)
  jump <- chkJump(sdf, 10)
  
  if (par && jump) return(TRUE)
  else return(FALSE)
}

# return true if the proportion of parity after merge does not exceed 10%
chkPar <- function(p1){
  prop <- table(p1$seq_par)[1]/sum(table(p1$seq_par))
  # if (length(table(p1$seq_par)) == 1) return(TRUE)
  # else if (min(prop, 1- prop) < 0.1) return(TRUE)
  # else return(FALSE)
  if(length(table(p1$seq_par)) == 1) return(TRUE)
  else if (min(table(p1$seq_par)[1], table(p1$seq_par)[2]) > 2) return(TRUE)
  else return(FALSE)
}

## return TRUE if both sequences have the same street name
## may be too restrictive
chkStreet <- function(p1){
  if(p1$street_add %>% unique() %>% na.omit %>% length != 1) return(FALSE)
  else return(TRUE)
}

## return TRUE if house number does not jump more than jump_size
chkJump <- function(p1, jump_size){
  x <- p1 %>% filter(!is.na(house_num)) %>% pull(house_num)
  #message(x)
  for(i in seq(1, length(x)-1)){
    if(abs(x[i] - x[i+1]) > jump_size) return(FALSE)
  }
  return(TRUE)
}

## return df with appended merge_SEQ column
appendMergeSeq <- function(sdf){
  seq_list <- sdf %>% 
    mutate(SEQ = as.numeric(SEQ)) %>% 
    pull(SEQ) %>% unique() %>% na.omit
  seq_list <- seq_list[which(!is.na(seq_list))]
  
  df_by_SEQ <- split( sdf , f = sdf$SEQ )
  mergeable <- c()
  for(i in seq(1, length(seq_list)-1)){
    mergeable[i]<-mergeSeq(rbind(df_by_SEQ[[i]], df_by_SEQ[[i+1]]))
  }
  
  appended_df <- sdf %>% mutate(merge_SEQ = SEQ)
  
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
HN6 <- appendSeqCol(HN5%>%mutate(house_num = as.numeric(house_num),
                                 i = row_number())) %>% fill(SEQ, .direction = "down")
HN7 <- appendMergeSeq(HN6) 

#save(HN7, file="HN7.RData")


# chkPar(p1)
# chkRoad(p1)
# chkJump(p1, 10)
# mergeSeq(p1)

