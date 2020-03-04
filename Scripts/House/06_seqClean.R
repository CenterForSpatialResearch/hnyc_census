

# SHOULD UPDATE MERGE_SEQ WHEN CHANGING NUMBER TOO (?)
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
  # ADD PARAMS TO ADJUST RESTRICTIVENESS, DO WE WANT CLEAN IN NEW COL?
  df <- df %>%
    filter(best_match == above_match & best_match == below_match
           & above_result %in% c(1, 2) & below_result %in% c(1, 2)) %>%
    mutate(new_hn = (above_hn1 + below_hn1) %/% 2) %>%
    select(index, new_hn)
  
  output <- left_join(seq_df, df, by = c("index" = "index"))
    
  return (output)
}