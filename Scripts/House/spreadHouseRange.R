## handle house ranges

sample_df <- HN5 %>% filter(dwelling_ser %in% c(22184, 22199, 22228))
for(i in seq(1, nrow(sample_df))){
  hn1 <- sample_df[i,"hn_1"]
  hn2 <- sample_df[i,"hn_2"]
  hn3 <- sample_df[i,"hn_3"]
  
  if(!is.na(hn3)){
    if(nchar(hn3)>nchar(hn1)){
      result <- padNumber(hn2, hn3)
      hn2 <- result[1]
      hn3 <-result[2]
    } else {
      result <- padNumber(hn1, hn2)
      hn1 <- result[1]
      hn2 <-result[2]
    }
  }
  if(is.na(hn2)) next
  else {
    result <- padNumber(hn1, hn2)
    hn1 <- result[1]
    hn2 <-result[2]
    
    if(!is.na(hn3)){
      result <- padNumber(hn1, hn2)
    } 
  }
}
hn_list <- sample_df$house_num

padNumber <- function(num_1, num_2){
  
  if(abs(num_1-num_2)<80 | nchar(num_1)==nchar(num_2)) return(c(num_1, num_2))
  
  if(nchar(num_1)>nchar(num_2)){
    return(c(num_1, paste0(substr(num_1, 1, nchar(num_1) - nchar(num_2)), num_2)))
  } else {
    return(c(paste0(substr(num_2, 1, nchar(num_2) - nchar(num_1)), num_1), num_2))
  }
}

padNumber(10,110)
