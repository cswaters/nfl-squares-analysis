library(ggplot2)
scores <- readr::read_csv('data/scores_by_quarter.csv')

p <- ggplot(scores,
       aes(factor(fav_last), factor(dog_last), 
           fill =pct_games, 
           label=pct_str)) +
  geom_tile(show.legend = FALSE) +
  geom_text(size = 4) +
  facet_wrap(vars(qtr_lbl),nrow = 2,scales = 'free') +
  scale_fill_distiller(direction = 1, palette = 'Greens') +
  labs(x = 'Favorite Score', 
       y = 'Underdog Score',
       title = 'NFL Score Combinations',
       subtitle = '2000 - 2018') +
  theme_minimal() +
  theme(panel.grid = element_blank())


ggsave('scores.png', plot = p)
