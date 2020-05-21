  
clean_hn_dict <- function(hn_dict) {

  fix_order <- function(str) {
    if (str == "NA?NA") {return (NA_character_)}
    
    str_split(str, "\\?") %>% 
      unlist() %>% 
      as.integer() %>%
      sort() %>% 
      str_c(collapse = "?")
  }
  
  hn_dict <- hn_dict %>%
    mutate(ED = str_pad(ED, 4, "left", pad = "0")) %>%
    select(ED, Name, Left_Low, Left_High, Right_Low, Right_High) %>%
    unite(col = "Left", Left_Low, Left_High, sep = "?") %>% 
    unite(col = "Right", Right_Low, Right_High, sep = "?") %>%
    rowwise() %>%
    mutate(Left = fix_order(Left), Right = fix_order(Right)) %>%
    ungroup() %>%
    gather(key = "column", value = "value", -c(ED, Name)) %>% 
    separate(col = value, into = c("Low", "High"), sep = "\\?") %>%
    mutate(Low = as.integer(Low), High = as.integer(High))
}