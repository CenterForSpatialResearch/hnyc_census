
## @knitr  evenly_fill_HN7

## taken from roundHouseNum.R
## round HN to the closest even (or odd) number
roundHouseNum <- function(hn, toEven = TRUE){
  if(toEven){
    return(2 * round(hn/2))
  } else{
    return(2*floor(hn/2)+1)
  }
}

# HN7 must exist
dummy <- HN7 %>% mutate(merge_SEQ = as.numeric(as.character(merge_SEQ)),
                        merge_SEQ = ifelse(is.na(merge_SEQ), -1, merge_SEQ))
seq_list <- dummy %>% pull(merge_SEQ) %>% unique() %>% as.numeric()

df_by_merge_seq <- split(dummy , f = dummy$merge_SEQ )

temp_list <- lapply(seq(1, length(seq_list)), function(s){
  
  if(seq_list[s]==-1){
    return(df_by_merge_seq[[s]] %>% mutate(hn_filled = NA, flg_hn_filled = NA, merge_SEQ = NA))
  }
  
  temp <- df_by_merge_seq[[s]]
  current_par <- temp %>% pull(seq_par) %>% na.omit %>% head(1)
  
  ## set the current parity
  if(current_par==0) current_par <- FALSE
  else current_par <- TRUE
  
  hn <- temp$house_num
  not_na_ind <- which(!is.na(hn))
  diff_ind <- diff(not_na_ind)
  
  hn_filled <- hn
  flg_hn_filled <- rep(0, length(hn))
  for(i in seq(1, length(diff_ind))){
    #message(i)
    step <- (hn[not_na_ind[i+1]] - hn[not_na_ind[i]])/diff_ind[i]
    #message(i, " ", diff_ind)
    if(length(diff_ind)==0) next()
    if(is.na(diff_ind)) next()
    if(diff_ind[i]==1) next()
    for(j in seq(1, diff_ind[i]-1)){
      hn_filled[not_na_ind[i]+j] <- roundHouseNum(hn[not_na_ind[i]] + j*step, current_par)
      flg_hn_filled[not_na_ind[i]+j] <- 1
      #print(hn[i] + j*step)
    }
  }
  
  temp["hn_filled"] <- hn_filled
  temp["flg_hn_filled"] <- flg_hn_filled
  
  return(temp)
  
})

## this is HN7 with new columns for filled in house numbers and their flags
filled_df <- do.call(rbind, temp_list)



