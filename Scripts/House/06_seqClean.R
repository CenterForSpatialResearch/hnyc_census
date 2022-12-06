#' This function creates a `new_hn` column with the corrected, non-breakpoint hn.
#' @param seq_df df with merge_SEQ column, sequence done WITHOUT check_st
#' @param too_small threshold of distinct hn in sequence for it to be considered too small
#' @param proximity how close could the prev and next hn be
#' @return original dataframe with `new_hn` column. `new_hn` is empty for non-breakpoint rows.
rmSeqBreakpoints = function(seq_df, too_small = 1, proximity = 2) {
  
  # create summary with start and endpoint of seq + start/end of seq above, start/end of seq below
  seq_hn_grp <- seq_df %>%
    group_by(merge_SEQ) %>%
    summarize(count = n_distinct(hn_1), start = first(hn_1), end = last(hn_1)) %>%
    arrange(as.numeric(merge_SEQ))
  
  above <- c(NA, flatten(seq_hn_grp[1:nrow(seq_hn_grp)-1, "end"])) %>%
    unlist()
  
  below <- c(flatten(seq_hn_grp[2:nrow(seq_hn_grp), "start"]), NA) %>%
    unlist()
  
  seq_hn_grp$above_end <- above
  seq_hn_grp$below_start <- below
  
  # pull problematic sequences: size is `too_small` and above and below are in close `proximity`
  narrow_breaks <- filter(seq_hn_grp,
                         count == too_small
                         & abs(above_end - below_start) <= proximity) %>%
    pull(merge_SEQ)
  
  # create df to store problematic sequences + details of record above and below it
  df <- seq_df %>%
    filter(merge_SEQ %in% narrow_breaks) %>%
    select(index, hn_1, hn_2, hn_3, best_match, result_type)
  
  above_idx = pull(df, index) - 1
  df_above <- seq_df %>%
    filter(index %in% above_idx) %>%
    mutate(idx_below = index + 1) %>%
    select(idx_below, above_hn1 = hn_1, above_hn2 = hn_2, above_hn3 = hn_3, 
           above_match = best_match, above_result = result_type)
  
  below_idx = pull(df, index) + 1
  df_below <- seq_df %>%
    filter(index %in% below_idx) %>%
    mutate(idx_above = index - 1) %>%
    select(idx_above, below_hn1 = hn_1, below_hn2 = hn_2, below_hn3 = hn_3, 
           below_match = best_match, below_result = result_type)

  df <- left_join(df, df_above, by = c("index" = "idx_below")) %>%
    left_join(df_below, by = c("index" = "idx_above"))
  
  # clean house number based on certain criteria

  df <- df %>%
    filter(best_match == above_match & best_match == below_match
           & above_result %in% c(1, 2) & below_result %in% c(1, 2)) %>%
    mutate(new_hn = (above_hn1 + below_hn1) %/% 2) %>%
    select(index, new_hn)
  
  output <- left_join(seq_df, df, by = c("index" = "index"))
    
  return (output)
}


#' This function cleans street names using sequence work. Details and demo in `sequence_EDA`.
#' @param df df with SEQ column, sequence done WITHOUT check_st.
#' @param res_limit limit to result type confidence. e.g. if set to 2 -> if the row below is result type = 2, it is considered a confident match.
#' @param diff_limit limit to the difference between above and below house numbers
#' @return original df. if changes are applied, `best_match` will be changed to the cleaned address, and result type will be changed to type 7.
cleanStusingSeq = function(seq_hn, res_limit = 2, diff_limit = 4) {
  multi_st_seq <- seq_hn %>%
    group_by(SEQ) %>%
    summarize(count = n_distinct(best_match)) %>%
    filter(count > 1)
  
  seq_w_multi_st_df = split(filter(seq_hn, SEQ %in% multi_st_seq$SEQ), pull(filter(seq_hn, SEQ %in% multi_st_seq$SEQ), SEQ))
  
  # applied to individual df containing hn of one SEQ
  cleanSeq = function(df) {
    above = df %>%
      select(abv_match = best_match, abv_res = result_type, abv_hn = hn_1)
    above = above[1:(nrow(above) - 1), ]
    above = bind_rows(tibble(abv_match = NA, abv_res = NA, abv_hn = NA),
                      above)
    
    below = df %>%
      select(bel_match = best_match, bel_res = result_type, bel_hn = hn_1)
    below = below[2:(nrow(below)), ]
    below = bind_rows(below,
                      tibble(bel_match = NA, bel_res = NA, bel_hn = NA))
    
    output = bind_cols(df, above, below) %>%
      mutate(result_type = ifelse(result_type > res_limit 
                                  & (abv_res <= res_limit | bel_res <= res_limit) 
                                  & ((abs(abv_hn - hn_1) <= diff_limit) | (abs(bel_hn - hn_1) <= diff_limit))
                                  & (abv_match == bel_match | is.na(abv_match) | is.na(bel_match)),
                                  7, result_type
      ),
      best_match = ifelse(result_type == 7, bel_match, best_match)) %>%
      filter(result_type == 7) %>%
      select("index", new_match = "best_match") %>%
      fill(new_match, .direction = "up") %>%
      fill(new_match, .direction = "down")
  
    return(output)  
  }
  
  # join back to the original df
  cleaned_dfs = map(seq_w_multi_st_df, function(x) cleanSeq(x)) %>%
    reduce(bind_rows)
  
  seq_hn = seq_hn %>%
    left_join(cleaned_dfs, by = c("index" = "index")) %>%
    mutate(best_match = ifelse(!is.na(new_match), new_match, best_match),
           result_type = ifelse(!is.na(new_match), 7, result_type)) %>%
    select(- c("new_match"))
  
  return(seq_hn) 
}

