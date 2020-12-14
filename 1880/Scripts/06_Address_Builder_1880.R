#' address_builder()
#' 
#' The function takes three variables `hn_1`, `best_match` and 
#' `street_type`, and concatenates an address suitable for geocoding. 
#' Two parameters are available for customising the output address. 
#' 
#' This is assissted by the street_type_builder() function, which
#' produces the `street_type` variable from the data frame.
#' 
#' * The first parameter `spell_ave` defaults to FALSE, 
#' and converts numerics to words for streets where `street_type` 
#' is avenue. For example, "3 AVE" will be transformed to "THIRD AVE".
#'  
#' * The second parameter `suffix` defaults to FALSE, and adds 
#' ordinal indicators (e.g. st, nd, rd) for all streets. 
#' For example, "108 ST" will be transformed to "108TH ST".


# Street type function
street_type_builder = function(df) {
  df$street_type = case_when(
    str_detect(df$best_match, pattern = " ST") ~ "ST",
    str_detect(df$best_match, pattern = " DR") ~ "DR",
    str_detect(df$best_match, pattern = " CIR") ~ "CIR",
    str_detect(df$best_match, pattern = " AVE") ~ "AVE",
    str_detect(df$best_match, pattern = " PL") ~ "PL",
    str_detect(df$best_match, pattern = " CT") ~ "CT",
    str_detect(df$best_match, pattern = " PARK") ~ "PARK",
    str_detect(df$best_match, pattern = " PLZ") ~ "PLZ",
    str_detect(df$best_match, pattern = " PKWY") ~ "PKWY",
    str_detect(df$best_match, pattern = " WAY") ~ "WAY",
    str_detect(df$best_match, pattern = " ALY") ~ "ALY",
    str_detect(df$best_match, pattern = " PIER") ~ "PIER",
    str_detect(df$best_match, pattern = "PIER") ~ "PIER",
    str_detect(df$best_match, pattern = " SLIP") ~ "SLIP",
    str_detect(df$best_match, pattern = " ROW") ~ "ROW",
    str_detect(df$best_match, pattern = " APPROACH") ~ "APPROACH",
    str_detect(df$best_match, pattern = " LN") ~ "LN",
    str_detect(df$best_match, pattern = " TER") ~ "TER",
    str_detect(df$best_match, pattern = " HTS") ~ "HTS",
    str_detect(df$best_match, pattern = " BLVD") ~ "BLVD",
    str_detect(df$best_match, pattern = " BRG") ~ "BRG",
    str_detect(df$best_match, pattern = " HL") ~ "HL",
    str_detect(df$best_match, pattern = "AVE") ~ "AVE",
    str_detect(df$best_match, pattern = "BROADWAY") ~ "AVE",
    TRUE ~ "ST")
  
  return(df)
}


# Address builder
build_Address = function(df, spell_ave = FALSE, suffix = FALSE) {
  
  # Variables used for Concatenate, replacing with empty string in order to sanitise input for functions below. Also because otherwise NA would be concatenated
  tmp_hn              = ifelse(!is.na(df$hn_1),
                               df$hn_1,
                               "")
  tmp_best_match      = ifelse(!is.na(df$best_match),
                               df$best_match,
                               "")
  tmp_street_type     = ifelse(!is.na(df$street_type),
                               df$street_type,
                               "")
  
  # Spell Avenue numerics if TRUE
  if (spell_ave == TRUE) {
    
    tmp_hn = ifelse(str_detect(tmp_street_type, 
                               pattern = "AVE"),
                    as.character(as.english(df$hn_1)),
                    tmp_hn)
  }
  
  # Adding suffixes to numerics if TRUE
  if (suffix == TRUE) {
    
    tmp_ordinal = str_extract(tmp_best_match, 
                              pattern = "[:digit:]+") %>%
      as.integer() %>%
      map_chr(., 
              function(x) {
                ifelse(!is.na(x),
                       scales::ordinal(x),
                       "")
              })
    
    tmp_best_match = str_replace(tmp_best_match, 
                                 pattern = "[:digit:]+",
                                 tmp_ordinal)
  }
  
  # Removing duplicated street type if street type already in best match for street types that are not "ST"
  tmp_street_type = ifelse(str_detect(tmp_best_match,
                                      pattern = paste0(tmp_street_type, "$")),
                           "",
                           tmp_street_type)
  
  # Concatenating Address
  addr = paste(tmp_hn, 
               tmp_best_match, 
               tmp_street_type) %>%
    str_trim(side = "both") %>%
    toupper()
  
  # Replacing empty strings back with NA
  addr = ifelse(addr == "" | str_detect(addr, pattern = "^ST$"),
                NA_character_,
                addr)
  # Appending to data frame
  df$addr = addr
  
  return(df)
}