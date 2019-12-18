
## @knitr round_HN

#' roundHouseNum
#'
#' This function rounds a numerical value to the closest even (odd) integer.
#' @param hn A numerical value to be rounded.
#' @param toEven A boolean whether a number should be rounded to the closest even integer. 
#' Set to `FALSE` if want to round to the closest odd integer. The default is `TRUE`.
#' @return Rounded integer.
roundHouseNum <- function(hn, toEven = TRUE){
  if(toEven){
    return(2 * round(hn/2))
  } else{
    return(2*floor(hn/2)+1)
  }
}

# ## return TRUE if num_1 is even
# isEven <- function(num_1){
#   if(num_1%%2==0) return(TRUE)
#   else return(FALSE)
# }
# 
# fillHN <- function(datafr, seq_num){
#   
#   x <- datafr %>% filter(SEQ == seq_num)
#   is_even <- x$house_num %>% unique() %>% head(1) %>% isEven()
#   m <- lm(house_num~i, data = x)
#   new_df <- x %>% select(i)
#   pred <- predict(object = m, newdata = new_df)
#   #new_df["raw_filled_house_num"] <- pred
#   new_df["round_filled_house_num"] <- roundHouseNum(pred, is_even)
#   
#   or <- x$house_num
#   new <- new_df$round_filled_house_num
#   
#   for(i in seq_along(or)){
#     if(is.na(or[i])){
#       or[i] <- new[i]
#     }
#   }
#   
#   x["filled_hn"] <- or
#   return(x %>% mutate(flg_filled_hn = ifelse(is.na(house_num), 1, 0)))
# }
# 
# ## return true if filled in HN breaks a seq of a datafr
# brokenSequence <- function(datafr){
#   s1 <- datafr$filled_hn %>% diff
#   s2 <- s1[which(s1 != 0)]
#   if(sum(s2>0) == 0 | sum(s2>0) == length(s2)) return(FALSE)
#   else return(TRUE)
# }



################
### working sample
################
# ## work on each seq. Fill in missing HNs and check if seq are broken.
# seq_list <- HN6 %>% pull(SEQ) %>% unique() %>% as.numeric() %>% na.omit()
# z <- lapply(seq_list, function(sq){
#   s <- fillHN(HN6, sq)
#   
#   brk <- brokenSequence(s)
#   
#   s["flg_brk_seq"] <- brk
#   return(s)
# })
# z1 <- do.call("rbind", z)
# #filled_page <- z1
# #save(filled_page, file = "Scripts/House/filled_page.Rdata")
# 
# z2 <- z1 %>% group_by(SEQ) %>% summarize(brk = mean(flg_brk_seq))

