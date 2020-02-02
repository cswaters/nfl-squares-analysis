source('utils.R')
source('load_libs.R')
# Armchair Analysis file path
f_paths <- file.path('~', 'Dropbox', 'ArmchairAnalysis', 'nfl_00-18')

# PxP data and Game data
pxp <- vroom::vroom(paste0(f_paths, '/PLAY.csv'))
game <- vroom::vroom(paste0(f_paths, '/GAME.csv'))

# get end of quarter scores
by_qtr <- pxp %>%
  group_nest(gid, qtr) %>%
  get_scores_by_qtr() %>%
  select(qtr, gid, off, def, ptso, ptsd) %>%
  filter(qtr < 5)

# get who is favorite
by_fav <- game %>%
  mutate(fav = ifelse(sprv >= 0, h, v),
         dog = ifelse(sprv >= 0, v, h)) %>%
  select(seas, gid, fav, dog)

rm(pxp, game)

combine_scr_sprds(by_qtr, by_fav, season = 2000) %>%
  mk_all_combos() %>%
  add_summary_stats() %>% 
  readr::write_csv(x = ., 
                   path = 'data/scores_by_quarter.csv')

rm(list = ls())
