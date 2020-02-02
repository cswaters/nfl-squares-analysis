get_last_digit <- function(s) {
  substr(s, nchar(s), nchar(s))
}

get_scores_by_qtr <- . %>%
  mutate(s1 = map_df(data, ~ tail(., 1)))

get_scores_by_qtr <- function(df) {
  df %>%
    mutate(s1 = map(data, ~ tail(., 1))) %>%
    tidyr::unnest(s1)
}

mk_all_combos <- function(df) {
  cross_df(.l = list(
    fav_last = c(0:9),
    dog_last = c(0:9),
    qtr = c(1:4)
  )) %>%
    left_join(df %>%
                mutate_if(is.character, as.integer)) %>%
    mutate_all( ~ ifelse(is.na(.), 0, .))
}

combine_scr_sprds <- function(df1, df2, season = 2000) {
  left_join(df1, df2) %>%
    mutate(
      fav_scr = ifelse(fav == off, ptso, ptsd),
      dog_scr = ifelse(fav == off, ptsd, ptso)
    ) %>%
    mutate(fav_last = get_last_digit(fav_scr),
           dog_last = get_last_digit(dog_scr)) %>%
    # cutoff season
    filter(seas >= season) %>%
    count(qtr, fav_last, dog_last, name = 'games')
}


add_summary_stats <- function(df) {
  df %>%
    group_by(qtr) %>%
    mutate(
      rank = min_rank(-games),
      pctile = percent_rank(games),
      pct_games = games / sum(games),
      pct_str = scales::percent(pct_games),
      qtr_lbl = factor(
        qtr,
        levels = c(1, 2, 3, 4),
        labels = c(
          'Score at end of 1st Quarter',
          'Score at end of 2nd Quarter',
          'Score at end of 3rd Quarter',
          'Score at end of 4th Quarter (No OT)'
        )
      )
    ) %>%
    ungroup()
}