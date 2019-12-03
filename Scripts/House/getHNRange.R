mn_nest <- readRDS("Scripts/House/data/mn_nest.rds")

## * use only column Name
## * skip gap within ranges

hnr_list <- lapply(seq(1, nrow(mn_nest)), function(e){
  
  temp <- mn_nest[e,]$data[[1]] %>% filter(Name != "NULL")
  
  hnr_left <- temp %>% select(Name, Left_Low, Left_High) %>%
    group_by(Name) %>%
    summarize(Left_Low = min(Left_Low, na.rm = TRUE), 
              Left_High = max(Left_High, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(L_par1 = ifelse(Left_Low%%2==0, "even", "odd"),
           L_par2 = ifelse(Left_High%%2==0, "even", "odd"),
           flg_L_par = ifelse(L_par1 != L_par2, 1, 0))
  
  hnr_right <- temp %>% select(Name, Right_Low, Right_High) %>%
    group_by(Name) %>%
    summarize(Right_Low = min(Right_Low, na.rm = TRUE), 
              Right_High = max(Right_High, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(R_par1 = ifelse(Right_Low%%2==0, "even", "odd"),
           R_par2 = ifelse(Right_High%%2==0, "even", "odd"),
           flg_R_par = ifelse(R_par1 != R_par2, 1, 0))
  
  hnr_df <- hnr_left %>% full_join(hnr_right, by = "Name") %>% mutate(ED = e)
})

hnr_df <- do.call(rbind, hnr_list)
#save(hnr_df, file = "Scripts/House/data/hnr_df.Rdata")


