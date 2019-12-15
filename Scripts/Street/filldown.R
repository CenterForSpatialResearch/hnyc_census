
#' filldown
#' 
#' Fill down cleaned street names within the same enumeration page and ED.
#' @param df A dataframe og household records with `microfilm`, `ED`, 
#' and `best_match` columns.
#' @return A dataframe with cleaned street filled down within an enumeration 
#' page and ED (group_by(`microfilm`, `ED`)).
filldown <- function(df){
  x <- df %>% group_by(microfilm, ED) %>%
    fill(best_match, .direction="down") %>%
    ungroup()
  return(x)
}

#' ## check if "P" records have a missing page num
#' ## @param df: the entire dataframe without any transformation
#' #' isMissingPageNum
#' #' 
#' #' Fill down cleaned street names within the same enumeration page and ED.
#' #' @param df A dataframe og household records with `microfilm`, `ED`, 
#' #' and `best_match` columns.
#' #' @return A dataframe with cleaned street filled down within an enumeration 
#' #' page and ED (group_by(`microfilm`, `ED`)).
#' isMissingPageNum <- function(df){
#'   x <- df %>% filter(`Record type`=="P", is.na(`Microfilm page number 3`))
#'   if(dim(x)[1]==0){return(FALSE)}
#'   else{return(TRUE)}
#' }
#' 
#' ## check if there are more than 2 missing pagenums in a row in the df
#' ## @param df: the entire dataframe without any transformation
#' isSingleNa <- function(df){
#'   ## max() is the number of NAs that occur next to other NAs.
#'   cons_cnt <- is.na(df %>% pull(`Microfilm page number 3`)) %>% rle
#'   result <- max(cons_cnt$lengths*cons_cnt$values)
#'   if(result >1){return(FALSE)}
#'   else{return(TRUE)}
#' }


# test
#x <-read_csv("../HNYC/_working/us1910m_usa_sample100k.csv")
#filldown(x) %>% View()
