### To-do
### 1. Check if fill up only fill one row up (H row)
### 2. Think of how to test the transformations
###        - the idea of a unit test is to see if a function works as expected
###        - compare expected vs. observed
###        1. write a function that wraps the tranfomation
###        2. create a simple dataframe to test

########################################
#### break into dataframes by microfilm page number
########################################

####### method 1 #########
#### fill down within a enumeration page ######

## 1. fill up the page number
##      - check if it messes something up**
## 2. break into df by enum page
## 3. fill down
## 4. rbind df back

start_time <- Sys.time()

raw_df <-read_csv("_working/us1910m_usa_sample100k.csv") %>% 
  fill(`Microfilm page number 3`, .direction = "up")
enum_page <- raw_df$`Microfilm page number 3` %>% unique() %>% na.omit()

break_and_filldown <- function(page){
  d <- raw_df %>% filter(`Microfilm page number 3`==page) %>% 
    fill(`HNYC_ID`:`Street address 2`, .direction="down") %>%
    filter(`Record type`=="P")
  return(d)
}

enum_df <- lapply(enum_page, break_and_filldown)
filled_df <- do.call("rbind", enum_df)
end_time <- Sys.time()

message("\nexecution time: ", end_time-start_time, "seconds")

#################################
####### method 2 ######### [faster]
start_time <- Sys.time()
new_df <-read_csv("_working/us1910m_usa_sample100k.csv") %>% 
  fill(`Microfilm page number 3`, .direction = "up") %>%
  group_by(`Microfilm page number 3`) %>%
  fill(`HNYC_ID`:`Street address 2`, .direction="down") %>%
  filter(`Record type`=="P") %>%
  ungroup()

fill_down <- function(df){
  x <- df %>% 
    fill(`Microfilm page number 3`, .direction = "up") %>%
    group_by(`Microfilm page number 3`) %>%
    fill(`HNYC_ID`:`Street address 2`, .direction="down") %>%
    filter(`Record type`=="P") %>%
    ungroup()
  return(x)
}

end_time <- Sys.time()

message("\nexecution time: ", end_time-start_time, "seconds")

#########################
all.equal(new_df, filled_df)

df <-read_csv("_working/us1910m_usa_sample100k.csv") %>% 
  fill(`Microfilm page number 3`, .direction = "up")
df %>% filter(`Microfilm page number 3`=="0011") %>% View()
df %>% filter(`Line number 2`=="100") %>% View()
df %>% filter(is.na(`Line number 2`)) %>% pull(`Record type`) %>% unique()
df %>% filter(!is.na(`Microfilm page number 3`)) %>% pull(`Microfilm page number 3`) %>% as.numeric %>% unique() %>% sort() %>% length

df <-read_csv("_working/us1910m_usa_sample100k.csv")
which(is.na(df %>% pull(`Microfilm page number 3`)))


## check if "P" records have a missing page num
## @param df: the entire dataframe without any transformation
is_missing_page_num <- function(df){
  x <- df %>% filter(`Record type`=="P", is.na(`Microfilm page number 3`))
  if(dim(x)[1]==0){return(FALSE)}
  else{return(TRUE)}
}

## check if there are more than 2 missing pagenums in the df
## @param df: the entire dataframe without any transformation
is_single_na <- function(df){
  ## max() is the number of NAs that occur next to other NAs.
  cons_cnt <- is.na(df %>% pull(`Microfilm page number 3`)) %>% rle
  result <- max(cons_cnt$lengths*cons_cnt$values)
  if(result >1){return(FALSE)}
  else{return(TRUE)}
}

is_single_na(df)
is_missing_page_num()

################
#### test fill_down() ####

test_df <- data.frame(`HNYC_ID`=seq(1,16),
                      `Record type`=c(rep("P",4),"H", rep("P",5),"H",rep("P",4),"H"),
                      `Microfilm page number 3`=c(rep(100, 4), NA, rep(100, 2), rep(101, 3), NA, rep(101, 4), NA),
                      `dwelling number`=c(rep(NA, 4), "1111", rep(NA, 5), "2222", rep(NA, 4), "3333"),
                      `Street address 2`=c(rep(NA, 4), "Madison", rep(NA, 5), "100thST", rep(NA, 4), "Broadway"))
names(test_df) <- c("HNYC_ID", "Record type","Microfilm page number 3", "dwelling number", "Street address 2")

expect <- data.frame(`HNYC_ID`=c(seq(1,4), seq(6,10), seq(12,15)),
                     `Record type`=c(rep("P",13)),
                     `Microfilm page number 3`=c(rep(100, 6), rep(101, 7)),
                     `dwelling number`=c(rep(NA, 4), rep("1111", 2), rep(NA, 3), rep("2222", 4)),
                     `Street address 2`=c(rep(NA, 4), rep("Madison", 2), rep(NA, 3),  rep("100thST", 4)))
names(expect) <- c("HNYC_ID", "Record type","Microfilm page number 3", "dwelling number", "Street address 2")

## observe has 2 levels in redcord type. filtering one out does not help
observe <- fill_down(test_df)
all.equal(expect, observe)
View(observe)


