# This script contains the full street address matching and fill down code.
# Items with [FLAG] = to check if still necessary in updated code.

# Libraries
library(kableExtra)
library(tidyverse)
library(fuzzyjoin)

# ---- DATA ---- 
# DATA
## Load Datasets
sample <- read_csv(here::here("Data/us1910m_usa_sample100k.csv"))
edict <- read_csv(here::here("Data/combined_edict_mn.csv"))

## clean column names
names(sample) <- c("hnyc_id", "record", "township", "county", "ED", "person_id",
                   "dwelling_seq", "dwelling_ser", "dwelling_ser2", "hh_seq", "hh_seq_8",
                   "hh_ser2", "hh_ser_bef_split", "indiv_seq", "split", 
                   "line_no", "line_no2", "microfilm",
                   "n_fam", "n_person_bef_split", "house_num", "street_add")

## [FLAG] clean ED number
edict <- edict %>%
  mutate(ED = str_pad(ED, 4, "left", pad = "0")) # ensure numbers are all 4 digit

## Extract Unique Addresses
unique_addresses <- sample %>%
  select(ED, street_add) %>%
  filter(!is.na(street_add)) %>%
  distinct(ED, street_add)

# ---- CLEAN ADDRESSES ----
## import rem_dup_word
rem_dup_word <- function(x){
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = 
          " ")
}

## [FLAG] Subsequent matching requires x to be a vector, not a df
str_clean<-function(x){
  x <- gsub("\\<SRT\\>$|\\<SR\\>$\\<SRT\\>$|\\<STR\\>$|\\<SST\\>$|\\<SEET\\>$|\\<TREET\\>$|\\<SHEER\\>$|\\<SHEE\\>$|\\<STREE\\>$|\\<SREET\\>$|\\<REET\\>$|\\<STEE\\>$|\\<ST\\>$","STREET",x)
  x <-gsub("\\<N\\>","NORTH",x)
  x<-gsub("\\<S\\>","SOUTH",x)
  x<-gsub("\\<E\\>","EAST",x)
  x<-gsub("\\<W\\>","WEST",x)
  x<-gsub("\\<DR\\>|\\<DV\\>|\\<DE\\>$|\\<DRV\\>|\\<DRI\\>|\\<DRIV\\>|\\<DRIE\\>","DRIVE",x) 
  x<-gsub("\\<CIR\\>|\\<CRL\\>|\\<CIRC\\>|\\<CR\\>|\\<CL\\>|\\<CIRCL\\>|\\<CICLE\\>","CIRCLE",x)
  x<-gsub("\\<AVE\\>|\\<AV\\>|\\<AVN\\>|\\<AVEN\\>|\\<AVENU\\>","AVENUE",x)
  x<-gsub("\\<CT\\>|\\<CRT\\>|\\<CTR\\>|\\<COUR\\>|\\<COT\\>|\\<CORT\\>","COURT",x)
  x<-gsub("\\<BLVD\\>|\\<BVLD\\>|\\<BV\\>|\\<BLD\\>|\\<BD\\>|\\<BL\\>|\\<BLV\\>","BOULEVARD",x)
  x<-gsub("\\<RD\\>|\\<RAD\\>|\\<ROD\\>","ROAD",x)
  x<-gsub("\\<ALY\\>|\\<AL\\>|\\<ALLY\\>|\\<ALEY\\>|\\<ALLE\\>|\\<AY\\>","ALLEY",x)
  x<-gsub("\\<PL\\>|\\<PLC\\>|\\<PLE\\>|\\<PC\\>|\\<PLAC\\>|\\<PLCE\\>|\\<PCE\\>","PLACE",x)
  x<-gsub("\\<PK\\>|\\<PRK\\>|\\<PRAK\\>|\\<PAK\\>","PARK",x)
  x<-gsub("\\<PKWY\\>|\\<PARKW\\>|\\<PWY\\>|\\<PKW\\>|\\<PRKWY\\>|\\<PKWY\\>|\\<PKW\\>","PARKWAY",x)
  x<-gsub("\\<APPR\\>|\\<APR\\>|\\<APPROA\\>|\\<APRCH\\>|\\<APPRCH\\>","APPROACH",x)
  x<-gsub("\\<TER\\>|\\<TERR\\>|\\<TRC\\>|\\<TRCE\\>|\\<TR\\>","TERRACE",x)
  x<-gsub("\\<PLZ\\>|\\<PLAZ\\>|\\<PZ\\>|\\<PLZA\\>","PLAZA",x)
  x<-gsub("\\<LN\\>|\\<LNE\\>|\\<LAN\\>","LANE",x)
  x<-gsub("\\<BRG\\>|\\<BRGD\\>|\\<BGE\\>","BRIDGE",x)
  x<-gsub("\\<HL\\>|\\<HLL\\>|\\<HIL\\>","HILL",x)
  x<-gsub("\\<HTS\\>|\\<HT\\>|\\<HEIGHT\\>|\\<HEGHTS\\>|\\<HHT\\>|\\<HEIGT\\>","HEIGHTS",x) 
  x<-gsub(".*\\((.*)\\).*", "\\1", x)
  x<-str_remove(x,"STREET")
  x<-gsub("\\d+\\ - *\\d*|\\d+\\ TO *\\d*|\\d+\\-\\d*","",x) #remove addresses
  
  ## dealing with numbered streets
  x<-gsub("(\\d)(ST|ND|RD|TH)\\b", "\\1", x)
  x<-str_remove(x, "(?<=[0-9])(ST|ND|RD|TH)")
  x<-gsub("\\<ONE HUNDRED\\>|\\<ONEHUNDRED\\>|\\<HUNDRED\\>|\\<HUDRED\\>|\\<HUNDED\\>","1",x) 
  x<-gsub("\\<TWO HUNDRED\\>|\\<TWOHUNDRED\\>","2",x)
  x<-gsub("-"," ",x)
  x<-gsub("\\<AND\\>"," ",x)
  x<-gsub("&"," ",x)
  x<-gsub("\\<1ST\\>|\\b1\\b","FIRST",x)
  x<-gsub("\\<2ND\\>|\\b2\\b","SECOND",x)
  x<-gsub("\\<3RD\\>|\\b3\\b","THIRD",x)
  x<-gsub("\\<4TH\\>|\\b4\\b","FOURTH",x)
  x<-gsub("\\<5TH\\>|\\b5\\b","FIFTH",x)
  x<-gsub("\\<6TH\\>|\\b6\\b","SIXTH",x)
  x<-gsub("\\<7TH\\>|\\b7\\b","SEVENTH",x)
  x<-gsub("\\<8TH\\>|\\b8\\b","EIGHTH",x)
  x<-gsub("\\<9TH\\>|\\b9\\b","NINTH",x)
  x<-gsub("\\<10TH\\>|\\b10\\b","TENTH",x)
  x<-gsub("\\<11TH\\>|\\b11\\b","ELEVENTH",x)
  x<-gsub("\\<12TH\\>|\\b12\\b","TWELFTH",x)
  x<-gsub("\\<13TH\\>|\\b13\\b","THIRTEENTH",x)
  x<-gsub("\\<14TH\\>|\\b14\\b","FORTEENTH",x)
  x<-gsub("\\<15TH\\>|\\b15\\b","FIFTEENTH",x)
  x<-gsub("\\<16TH\\>|\\b16\\b","SIXTEENTH",x)
  x<-gsub("\\<17TH\\>|\\b17\\b","SEVENTEENTH",x)
  x<-gsub("\\<18TH\\>|\\b18\\b","EIGHTEENTH",x)
  x<-gsub("\\<19TH\\>|\\b19\\b","NINETEENTH",x)
  x<-gsub("\\<TWENTY\\>|\\<TWENTI\\>|\\<TENTI\\>","2",x)
  x<-gsub("\\<THIRTY\\>|\\<THIRTHY\\>|\\<THIRTEY\\>|\\<TIRTY\\>|\\<TRITHY\\>","3",x)
  x<-gsub("\\<FORTY\\>|\\<FOURTY\\>|\\<FOURTHY\\>|\\<FRTY\\>","4",x)
  x<-gsub("\\<FIFTY\\>|\\<FIFTEY\\>|\\<FIFT\\>|\\<FITY\\>|\\<FIFTHY\\>","5",x)
  x<-gsub("\\<SIXTY\\>|\\<SXTY\\>|\\<SIXY\\>|\\<SXTY\\>|\\<SIXTHY\\>|\\<SIXTEY\\>","6",x)
  x<-gsub("\\<SEVENT\\>|\\<SEVENTY\\>|\\<SEVENTEY\\>|\\<SVENTY\\>|\\<SEVENTI\\>","7",x)
  x<-gsub("\\<EIGHTY\\>|\\<EIGHTEY\\>|\\<EIGHTE\\>","8",x)
  x<-gsub("\\<UNITY\\>|\\<NINTH\\>|\\<NINETY\\>|\\<NINETEY\\>|\\<NINETIETH\\>|\\<NINTY\\>","9",x)
  x<-gsub("\\<FRIST\\>|\\<FIST\\>|\\<FRST\\>|\\<FIRST\\>|\\<ONE\\>","1",x)
  x<-gsub("\\<SECOND\\>|\\<SECORD\\>|\\<SCOND\\>|\\<SECOND\\>|\\<TWO\\>","2",x)
  x<-gsub("\\<THRID\\>|\\<THIRD\\>|\\<TIRD\\>|\\<TRIHD\\>|\\<THREE\\>","3",x)
  x<-gsub("\\<FORTH\\>|\\<FOURTH\\>|\\<FROTH\\>|\\<FROUTH\\>|\\<FOUR\\>","4",x)
  x<-gsub("\\<FIFETH\\>|\\<FIFTH\\>|\\<FIFFTH\\>|\\<FIFTHE\\>|\\<FIVE\\>","5",x)
  x<-gsub("\\<SIXTH\\>|\\<SXTH\\>|\\<SITH\\>|\\<SIHXT\\>|\\<SIX\\>","6",x)
  x<-gsub("\\<SEVENTH\\>|\\<SVEN\\>|\\<SVENTH\\>|\\<SEVENH\\>|\\<SEVENT\\>|\\<SEVEN\\>","7",x)
  x<-gsub("\\<EIGHTH\\>|\\<EIGHTEH\\>|\\<EITH\\>|\\<EIGHT\\>","8",x)
  x<-gsub("\\<NINETH\\>|\\<NINTH\\>|\\<NINT\\>|\\<NINETH\\>|\\<NINE\\>|\\<NIN\\>","9",x)
  x<-gsub("\\<TWENTIETH\\>|\\<TWENTIEFTH\\>","20",x) #NEW
  x<-gsub("\\<THIRTIETH\\>|\\<THIRTIEFTH\\>","30",x)
  x<-gsub("\\<FORTIETH\\>|\\<FOURTIETH\\>","40",x)
  x<-gsub("\\<FIFTIETH\\>","50",x)
  x<-gsub("\\<SIXTIETH\\>","60",x)
  x<-gsub("\\<SEVENTIETH\\>","70",x)
  x<-gsub("\\<EIGHTIETH\\>","80",x)
  x<-gsub("\\<NINETIETH\\>|\\<NINTIETH\\>","90",x)
  x<-gsub("(?<=\\d) (?=\\d)","",x,perl = T) #new close gaps between all numbers
  ## place names
  ##x<-gsub("\\bSTR\\b","", x)
  x<-gsub("^\\bST\\b","SAINT", x) 
  x<-gsub("\\bHOUSE\\b","", x)
  x<-gsub("\\bHOSTEL\\b","", x)
  x<-gsub("\\bHOTEL\\b","", x)
  x<-gsub("\\bLODGE\\b","", x)
  x<-gsub("\\bLODGING\\b","", x)
  x<-trimws(x, "both")
  x<-gsub("\\<N\\>","NORTH",x)
  ##x<-gsub("\\<ST\\>","",x)
  ##x<-gsub("\\<STREET\\>","",x)
} 

## preallocate memory for cleaned column
cleaned <- rep(NA_character_, nrow(unique_addresses))

## apply rem_dup_word() and str_clean()
for (i in 1:nrow(unique_addresses)) {
  cleaned[i] <- toupper(rem_dup_word(unique_addresses[i, 2]))
  cleaned[i] <- str_clean(cleaned[i])
}

# ---- FIRST MATCH ----
## create add_matches, which combines `unique_addresses` with the `cleaned` column
add_matches <- tibble(ED = unique_addresses$ED,
                      raw = unique_addresses$street_add,
                      clean = cleaned)


## create str_algo, which has all the methods we will use
str_algo <- c("dl", "qgram", "cosine", "jaccard", "jw")

## create empty lists which will store outputs
algo_list <- list()
ed_list <- list()

## matching
for (i in unique(add_matches$ED)) {
  # create subset of edict and add_matches for a particular ED
  # TO ACCEPT BK: ED_dict <- filter(edict, ED == i & MNBK == "BK") %>%
  ED_dict <- filter(edict, ED == i) %>% 
    select(- "ED") %>%
    unlist() 
  ED_dict <- data.frame(clean = ED_dict, stringsAsFactors = FALSE) %>%
    filter(!is.na(clean))
  ED_add <- filter(add_matches, ED == i)
  
  for (j in str_algo) {
    # stringdist_join, extract best match for each method
    result <- stringdist_left_join(ED_add, ED_dict, by = "clean", 
                                   max_dist = 5, method = j, distance_col = "dscore") %>%
      select(ED, raw, clean.x, clean.y, dscore) %>%
      group_by(raw) %>%
      arrange(dscore) %>%
      slice(1)
    
    # at this point, result = dataframe of addresses in ED (i), merged with method (j)
    # append result to algo_list, each element in algo_list is a dataframe
    algo_list[[j]] <- result
    
    # "column bind" all elements in algo_list to form a df called ed_df
    # ed_df now contains all addresses in ED(i) with matches using all methods
    ed_df <- algo_list %>% 
      reduce(left_join, by = c("ED", "raw", "clean.x"))
  }
  # append each ed_df to ed_list 
  ed_list[[i]] <- ed_df
}

## bind all dfs by row
match_dict <- bind_rows(ed_list) %>%
  select(ED = "ED", raw = "raw", clean = "clean.x",
         match_dl = "clean.y.x", dscore_dl = "dscore.x",
         match_qgram = "clean.y.y", dscore_qgram = "dscore.y",
         match_cos = "clean.y.x.x", dscore_cos = "dscore.x.x",
         match_jac = "clean.y.y.y", dscore_jac = "dscore.y.y",
         match_jw = "clean.y", dscore_jw = "dscore")

## ---- FURTHER TUNING ----
## create summary statistics
summ_dscores <- match_dict %>%
  replace(is.na(.), 5) %>%
  group_by(ED) %>%
  summarize(mean_dl = mean(dscore_dl), sd_dl = sd(dscore_dl),
            mean_qgram = mean(dscore_qgram), sd_qgram = sd(dscore_qgram),
            mean_cos = mean(dscore_cos), sd_cos = sd(dscore_cos),
            mean_jac = mean(dscore_jac), sd_jac = sd(dscore_jac),
            mean_jw = mean(dscore_jw), sd_jw = sd(dscore_jw)) %>%
  replace(is.na(.), 0)

## format into a list such that e.g. `summ_dscores_list$0010$jw$mean` gives mean jw dscore of ED 0010
summ_dscores_list <- unique(summ_dscores$ED) %>% as.list()
names(summ_dscores_list) <- unique(summ_dscores$ED)

for (i in names(summ_dscores_list)) {
  s <- filter(summ_dscores, ED == i)
  ED_sum_list <- list(dl = list(mean = s$mean_dl, sd = s$sd_dl),
                      qgram = list(mean = s$mean_qgram, sd = s$sd_qgram),
                      cosine = list(mean = s$mean_cos, sd = s$sd_cos),
                      jaccard = list(mean = s$mean_jac, sd = s$sd_jac),
                      jw = list(mean = s$mean_jw, sd = s$sd_jw))
  summ_dscores_list[[i]] <- ED_sum_list
}

## match again
for (i in unique(add_matches$ED)) {
  # create subset of edict and add_matches for a particular ED
  # TO ACCEPT BK: ED_dict <- filter(edict, ED == i & MNBK == "BK") %>%
  ED_dict <- filter(edict, ED == i) %>% 
    select(- "ED") %>%
    unlist() 
  ED_dict <- data.frame(clean = ED_dict, stringsAsFactors = FALSE) %>%
    filter(!is.na(clean))
  ED_add <- filter(add_matches, ED == i)
  
  for (j in str_algo) {
    # set a max_dist that varies by ED. max_dist is 2sd higher than mean.
    threshold <- summ_dscores_list[[i]][[j]]$mean + 2 * summ_dscores_list[[i]][[j]]$sd
    
    # stringdist_join, extract best match for each method
    result <- stringdist_left_join(ED_add, ED_dict, by = "clean", 
                                   max_dist = threshold, method = j, distance_col = "dscore") %>%
      select(ED, raw, clean.x, clean.y, dscore) %>%
      group_by(raw) %>%
      arrange(dscore) %>%
      slice(1)
    
    # at this point, result = dataframe of addresses in ED (i), merged with method (j)
    # append result to algo_list, each element in algo_list is a dataframe
    algo_list[[j]] <- result
    
    # "column bind" all elements in algo_list to form a df called ed_df
    # ed_df now contains all addresses in ED(i) with matches using all methods
    ed_df <- algo_list %>% 
      reduce(left_join, by = c("ED", "raw", "clean.x"))
  }
  # append each ed_df to ed_list 
  ed_list[[i]] <- ed_df
}

## bind all dfs by row
match_dict <- bind_rows(ed_list) %>%
  select(ED = "ED", raw = "raw", clean = "clean.x",
         match_dl = "clean.y.x", dscore_dl = "dscore.x",
         match_qgram = "clean.y.y", dscore_qgram = "dscore.y",
         match_cos = "clean.y.x.x", dscore_cos = "dscore.x.x",
         match_jac = "clean.y.y.y", dscore_jac = "dscore.y.y",
         match_jw = "clean.y", dscore_jw = "dscore")

# ---- BEST MATCH ----
## function to extract modes
modal <- function(x) {
  freq <- table(x) %>% as.data.frame(stringsAsFactors = FALSE)
  result <- freq[which(freq$Freq == max(freq$Freq)), 1]
  if (length(result) > 1){
    result <- str_c(result, collapse = "+")
  }
  result
}

## extract modes
match_dict_modes <- match_dict %>%
  replace(is.na(.), "0") %>%
  select(- c("dscore_dl", "dscore_qgram", "dscore_cos", "dscore_jac", "dscore_jw")) %>%
  gather("method", "match", - c("ED", "raw", "clean")) %>%
  group_by(ED, raw) %>%
  summarise(mode = modal(match))

## join to match_dict
match_dict <- left_join(match_dict, match_dict_modes, by = c("ED", "raw"))

## best match
match_dict <- match_dict %>%
  replace(is.na(.), 0) %>%
  rowwise() %>%
  mutate(result_type = ifelse(match_dl == match_cos & match_dl == match_qgram & match_dl == match_jac & match_dl == match_jw, ifelse(clean == match_dl, 1, ifelse(mode == 0, 6, 2)), ifelse(str_detect(mode, "\\+"), 4, ifelse(mode == 0, 5, 3)))) %>%
  mutate(best_match = case_when(result_type %in% c(1, 2, 3) ~ mode,
                                result_type %in% c(4, 5, 6) ~ "0"))

# ---- FILL DOWN ----
## create flags for row that had to be filled
match_dict <- mutate(match_dict, flag = ifelse(best_match == 0, 1, 0))
match_dict$index <- 1:nrow(match_dict)

na_matches <- filter(match_dict, best_match == 0)

## fill down
for (i in na_matches$index) {
  # extract pool of potential matches
  i <- as.numeric(i)
  match_ED <- na_matches[na_matches$index == i, "ED"]
  pool_indexes <- c(i - 3, i - 2, i - 1, i + 1, i + 2, i + 3)
  pool_matches <- filter(match_dict, index %in% pool_indexes & ED == match_ED) %>%
    select(best_match) %>%
    distinct()
  
  # match using jw
  match <- stringdist_left_join(filter(na_matches, index == i), pool_matches, by = c(clean = "best_match"), 
                                max_dist = 5, method = "jw", distance_col = "dscore") %>%
    group_by(raw) %>%
    arrange(dscore) %>%
    slice(1)
  
  # sub match into match_dict
  match_dict[match_dict$index == i, "best_match"] <- match$best_match.y
}

# ---- MERGE TO ORIGINAL SAMPLE ----
## [FLAG] should decide which columns we want
match_dict_subset <- select(match_dict, ED, raw, best_match, flag)
sample_cleaned <- left_join(sample, match_dict_subset, by = c("ED" = "ED", "street_add" = "raw"))