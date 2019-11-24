## merge sequences

sample_df <-  HN6 %>% select(ED, house_num, street_add, SEQ, seq_par)

s1_df <- sample_df %>% filter(SEQ == 1)
s2_df <- sample_df %>% filter(SEQ == 2)

p1 <- rbind(s1_df, s2_df)
table(p1$seq_par) 

## return TRUE if 2 sequences in df can be merged into 1
## HAVE NOT TURN STREET CHECK ON
mergeSeq <- function(df){
  
  if(df$SEQ %>% unique() %>% length != 2){
    message("must provide 2 sequences")
    exit()
  }
  par <- chkPar(df)
  str <- chkStreet(df)
  jump <- chkJump(df, 10)
  
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

seq_list <- HN6 %>% pull(SEQ) %>% unique() %>% na.omit

## return yes if seq i can be joined with seq i+1
merge_list <- sapply(seq(1,length(seq_list)-1), function(i){
  s_df <- sample_df %>% filter(SEQ %in% c(seq_list[i], seq_list[i+1]))
  mergeSeq(s_df)
})

HN7 <- HN6 %>% mutate(merge_SEQ = SEQ)
for(i in seq(2, length(seq_list))){
  if(merge_list[i-1]){
    HN7 <- HN7 %>% mutate(merge_SEQ = ifelse(SEQ == seq_list[i], NA, merge_SEQ))
  }
}
HN7 <- HN7 %>% fill(merge_SEQ, .direction = "down")
HN7 %>% filter(merge_SEQ == 12) %>% View

HN7 %>% pull(SEQ) %>% unique() %>% na.omit() %>% length
HN7 %>% pull(merge_SEQ) %>% unique() %>% na.omit() %>% length

chkPar(p1)
chkRoad(p1)
chkJump(p1, 10)
mergeSeq(p1)

