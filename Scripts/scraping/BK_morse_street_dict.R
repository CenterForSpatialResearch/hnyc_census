##Brooklyn
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
bk1910url_base <- "https://stevemorse.org/census/unifiedstreets2.php?year=1910&state=NY&fullcity=Brooklyn&city=BK&ed=23"

##Load data
bk1910_links <- read_csv("streets_links_morse_1910BK.csv")
mn1910_links <- read_csv("streets_links_morse_1910MN.csv")

##Scaling up to multiple links
mn_link_vector <- c(mn1910_links$morse_streets_link)
bk_link_list <- data.frame("streets_link" = bk1910_links$morse_streets_link, "ed_number" = bk1910_links$ed_number, stringsAsFactors = FALSE)
bk_link_list_small <- data.frame(head(bk_link_list,11))

#######################################################
## A Function to Scrape Morse Street Names by ED page, clean them and format in df
scrapeArticle <- function(links_list){
    bkurl <- sprintf("https://stevemorse.org/census/unifiedstreets2.php?year=1910&state=NY&fullcity=Brooklyn&city=BK&ed=23-%s", links_list)
    ##mnurl <- sprintf("https://stevemorse.org/census/unifiedstreets2.php?year=1910&state=NY&fullcity=Manhattan&city=MA&ed=30-%s", links_list)
    nodes <- read_html(bkurl) %>%
        html_node("body")

    morse_text <- as.character(nodes) %>%
        str_replace_all("<body>\n\nStreets in 1910 ED 23-\\d+ in Brooklyn, NY<br><br>", "") %>%
        gsub(pattern = "<br>\n</body>", replacement = "")

    street_names <- strsplit(morse_text, split = "<br>")[[1]]
    tibble(street_names, ed = links_list)
}

##Pass Link List to Scrape Article with PURRR
DF <- bk_link_list$ed_number %>% ##as.list() %>%
    purrr::map_df(~scrapeArticle(.x))

##group and spread by ED (Kyi)
BK_EDstreet_dict <- DF %>%
    dplyr::arrange(street_names)%>%
    melt(., id.vars=c("street_names"), variable.name="ed")%>%
    select(-c(ed))%>%
    distinct(street_names, value)%>%
    dplyr::group_by(value)%>%
    dplyr::mutate(rn = paste0("V", 1:n()))%>%
    tidyr::spread(rn, street_names)
##Add "ED" as Column 1 Name
colnames(BK_EDstreet_dict)[1]="ED"
##Column order
BK_EDstreet_dict <- BK_EDstreet_dict[mixedorder(colnames(BK_EDstreet_dict))]
##Save to CSV
write_csv(BK_EDstreet_dict, "BK_EDstreet_dict.csv")
######################################################


