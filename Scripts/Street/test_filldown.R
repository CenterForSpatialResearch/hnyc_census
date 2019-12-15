library(testthat)
library(tidyverse)

sample_df <-read_csv("_working/us1910m_usa_sample100k.csv")

sample_df %>% filter(`Record type`=="H") %>% 
  pull(`Number of person records in household, before large group quarters were split up  (100% datasets)`)
  
## there are some rows with `Number of person records ...` =179
sample_df %>% 
  fill(`Microfilm page number 3`, .direction = "up") %>% 
  filter(`Number of person records in household, before large group quarters were split up  (100% datasets)`==179) %>% View()
y <- sample_df %>%
  fill(`Microfilm page number 3`, .direction = "up") %>% 
  filter(`Microfilm page number 3`=="0227") %>% View()
sum()
sample_df %>% filter(`Record type`=="P") %>% nrow()
  arrange(`Microfilm page number 3`) %>%
  View()
  

output <- filldown(sample_df)

### test fillup
test_that("fillup", {
  
  # this unit overall tests if fill up algorithm can be applied
  
  context("all H recods have NA page num")
  output <- sample_df %>% filter(`Record type`=="H") %>% 
    pull(`Microfilm page number 3`) %>% unique()
  expect_equal(is.na(output), TRUE)
  
  context("all P recods have page num")
  output <- sample_df %>% filter(`Record type`=="P", is.na(`Microfilm page number 3`)) %>% 
    pull(`Microfilm page number 3`)
  expect_equal(length(output), 0)
  
  context("No consecutive H records")
  index_diff <- which((sample_df %>% pull(`Record type`))=="H") %>% diff(lag=1)
  expect_equal(length(which(index_diff==1)), 0)
  
})

## the result from the fill-down algorithm is that only the top of each enumeration
## page is NAs, if there is any. This test checks that.
test_that("Only top page is NA", {
  
  ## There must be NAs continuously if there is any.
  page <- output %>% pull(`Microfilm page number 3`) %>% unique()
  temp <- sapply(page, function(p){
    
    ## check column `Standardized township (string)` as an example
    na_list <- which(is.na(output %>% filter(`Microfilm page number 3`==p) %>% 
                             pull(`Standardized township (string)`)))
    
    if (length(na_list) == 0){
      na_list=c(1,2)
    }
    
    context("If there is any NA, it starts from 1")
    expect_equal(1 %in% na_list, TRUE)
    
    
    context("NAs are consecutive")
    ## e.g. index = 2,3,8 fails the test
    # check for consecutive NAs, if there is any, from the start of the page
    expect_equal(diff(na_list) %>% sum, length(na_list)-1)
    
    ## the first NA must be in the first row
    invisible(expect_equal(na_list[1],1))
  })
})


## test if only "P" type is in the table
test_that("Only P type exists",{
  expect_that(output %>% 
                pull(`Record type`) %>% 
                unique(),
              equals("P"))})

## test the function with small dataframes
test_that("filldown works as expected",{
  
  test_df <- data.frame(`HNYC_ID`=seq(1,16),
                        `Record type`=c(rep("P",4),"H", rep("P",5),"H",rep("P",4),"H"),
                        `Microfilm page number 3`=c(rep(100, 4), NA, rep(100, 2), rep(101, 3), NA, rep(101, 4), NA),
                        `dwelling number`=c(rep(NA, 4), "1111", rep(NA, 5), "2222", rep(NA, 4), "3333"),
                        `Street address 2`=c(rep(NA, 4), "Madison", rep(NA, 5), "100thST", rep(NA, 4), "Broadway"),
                        stringsAsFactors = FALSE)
  names(test_df) <- c("HNYC_ID", "Record type","Microfilm page number 3", "dwelling number", "Street address 2")
  
  expect <- data.frame(`HNYC_ID`=c(seq(1,4), seq(6,10), seq(12,15)),
                       `Record type`=c(rep("P",13)),
                       `Microfilm page number 3`=c(rep(100, 6), rep(101, 7)),
                       `dwelling number`=c(rep(NA, 4), rep("1111", 2), rep(NA, 3), rep("2222", 4)),
                       `Street address 2`=c(rep(NA, 4), rep("Madison", 2), rep(NA, 3),  rep("100thST", 4)),
                       stringsAsFactors = FALSE)
  names(expect) <- c("HNYC_ID", "Record type","Microfilm page number 3", "dwelling number", "Street address 2")
  
  expect_equivalent(test_df %>% filldown, expect)
})
