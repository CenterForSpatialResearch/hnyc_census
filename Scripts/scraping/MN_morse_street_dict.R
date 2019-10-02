##Manhattan
##Loading Packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(rvest)
library(lubridate)
library(purrr)
library(reshape2)
library(gtools)

##Link vars
mn1910_url_base <- "https://stevemorse.org/census/unifiedstreets2.php?year=1910&state=NY&fullcity=Manhattan&city=MA&ed=30"

##Load data
mn1910_links <- read_csv("streets_links_morse_1910MN.csv")

##Scaling up to multiple links
mn_link_vector <- c(mn1910_links$morse_streets_link)
mn_link_list <- data.frame("streets_link" = mn1910_links$morse_streets_link, "ed_number" = mn1910_links$ed_number, stringsAsFactors = FALSE)
mn_link_list_small <- data.frame(head(mn_link_list,11))

#######################################################
## A Function to Scrape Morse Street Names by ED page, clean them and format in df
scrapeArticle <- function(links_list){
    mnurl <- sprintf("https://stevemorse.org/census/unifiedstreets2.php?year=1910&state=NY&fullcity=Manhattan&city=MA&ed=30-%s", links_list)
    nodes <- read_html(mnurl) %>%
        html_node("body")
    
    morse_text <- as.character(nodes) %>%
        str_replace_all("<body>\n\nStreets in 1910 ED 30-\\d+ in Manhattan, NY<br><br>", "") %>%
        gsub(pattern = "<br>\n</body>", replacement = "")
    
    street_names <- strsplit(morse_text, split = "<br>")[[1]]
    tibble(street_names, ed = links_list)
}

##Pass Link List to Scrape Article with PURRR
DF <- mn_link_list$ed_number %>% ##as.list() %>%
    purrr::map_df(~scrapeArticle(.x))

##group and spread by ED (Kyi)
MN_EDstreet_dict <- DF %>%
    dplyr::arrange(street_names)%>%
    melt(., id.vars=c("street_names"), variable.name="ed")%>%
    select(-c(ed))%>%
    distinct(street_names, value)%>%
    dplyr::group_by(value)%>%
    dplyr::mutate(rn = paste0("V", 1:n()))%>%
    tidyr::spread(rn, street_names)
##Add "ED" as Column 1 Name
colnames(MN_EDstreet_dict)[1]="ED"
##Column order
MN_EDstreet_dict <- MN_EDstreet_dict[mixedorder(colnames(MN_EDstreet_dict))]
##Save to CSV
write_csv(MN_EDstreet_dict, "MN_EDstreet_dict.csv")
######################################################