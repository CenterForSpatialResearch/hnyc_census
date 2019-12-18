
## @knitr  lookAround

#' lookAround
#'
#' The function returns a subset of a dataframe. A subset contains row `i`-`window_size`/2 to row `i`+`window_size`/2.
#' @param df A dataframe to be subset. It must contain colum `i`.
#' @param ind An index of a dataframe. This is the middle of the window.
#' @param window_size An integer specifying how many rows around the record should 
#' be inspected. This is just a rough number.
#' @return A subset dataframe of size `window_size`.
lookAround <- function(df, ind, window_size){
  half <- window_size/2
  return(df %>% filter(i %in% seq(ind-half, ind+half)))
}

