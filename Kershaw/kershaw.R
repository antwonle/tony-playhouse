library(dplyr)
library(ggplot2)
library(tibble)

regular = read.csv('kershaw_regular.csv', header = TRUE)
post = read.csv('kershaw_post.csv', header = TRUE)

#Making Data into Tibbles
regular = as_tibble(regular)
post = as_tibble(post)

#Filtering out data  to years Dodgers went to playoffs
regular = dplyr::select(regular, X, ERA, AVG)
regular = filter(regular, X != 2010, 2011, 2012)
regular = rename(regular, Season = X)

post = dplyr:: select(post, Season, ERA, AVG)

#Merging data frames
kershaw = merge(regular, post, by = 'Season')
kershaw = rename(kershaw, era_reg = ERA.x, avg_reg = AVG.x, era_post = ERA.y, avg_post = AVG.y, season = Season)

#Filtering out Career Average
kershaw_data = filter(kershaw, season != 'MLB Career')

#Plotting ERA in regular season and post season
era_p = ggplot(data = kershaw_data, aes(x = season, group = 1)) + geom_point(aes(y = era_reg, color = 'Regular Season'), size = 2) + 
  geom_line(aes(y = era_reg, color = 'Regular Season')) +
  geom_point(aes(y = era_post, color = 'Post Season'), size = 2) +
  geom_line(aes(y = era_post, color = 'Post Season')) +
  xlab('Season') + ylab('ERA') + ggtitle('Clayton Kershaw ERA During Regular and Post Season') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        plot.title = element_text(size = 20, hjust = 0.5)) 


#Plotting batter average against Kersgaw in regular and post season
avg_p = ggplot(data = kershaw_data, aes(x = season, group = 1)) + geom_point(aes(y = avg_reg, color = 'Regular Season'), size = 2) + 
  geom_line(aes(y = avg_reg, color = 'Regular Season')) +
  geom_point(aes(y = avg_post, color = 'Post Season'), size = 2) +
  geom_line(aes(y = avg_post, color = 'Post Season')) +
  xlab('Season') + ylab('AVG') + ggtitle('Clayton Kershaw Batting Average During Regular and Post Season') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        plot.title = element_text(size = 20, hjust = 0.5))

#Plotting both graphs 
gridExtra::grid.arrange(era_p, avg_p, ncol = 2)


#Creating dataframe for average mlb ERA
mlb_era = read.csv('mlb_avg_era.csv', header = TRUE)
mlb_era = as_tibble(mlb_era)

mlb_era_p = ggplot(data = mlb_era, aes(x = season, group = 1)) + geom_line(aes(y = reg_era, color = 'Regular Season')) + 
  geom_line(aes(y = post_era, color = 'Post Season')) + geom_point(aes(y = reg_era, color = 'Regular Season'), size = 2) +
  geom_point(aes(y = post_era, color = 'Post Season'), size = 2) +
  xlab('Season') + ylab('ERA') + ggtitle('MLB Average ERA in Regular and Post Season') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        plot.title = element_text(size = 20, hjust = 0.5)) 


#Comparing Kershaws ERA of MLB's average to Kershaw







