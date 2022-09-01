#mini game

avatar <- data %>% 
  filter(event_category %in% c("AspirationalAvatar", "Avatar Creation")) %>% 
  select(row_id, event_id, event_category, event_description, event_time_dbl)


temp <- as.data.frame(data[data$row_id== 972223,])
temp

#########################################
library(tidyverse)
library(dplyr)

full_data <- read_csv("logs.csv")

avatar_creation_ag <- full_data %>% 
  filter(event_id== 602) %>% 
  select(row_id, player_id, event_id, event_description, event_time_dbl, avatar_age, avatar_gender)

avatar_creation_r <- full_data %>% 
  filter(event_id== 604) %>% 
  select(row_id, player_id, event_id, event_description, event_time_dbl, avatar_id)

av_agr <- merge(x= avatar_creation_ag, y = avatar_creation_r, by ="player_id", all = TRUE) %>% 
  select(player_id, avatar_age, avatar_gender, avatar_id)

av_agr$avatar_id <- as.numeric(av_agr$avatar_id)

#plot(av_agr$avatar_age~av_agr$avatar_id)


### multiple linear regression

temp <- full_data %>% 
  filter(event_description == "Player sees lose panel") %>% 
  select(player_id, row_id, event_id, session, event_description) %>%
  group_by(player_id) %>% 
  count() #returns number of time player sees lose panel

av_agrl <- merge(x = av_agr, y = temp, by = "player_id", all = TRUE) %>% 
  select(player_id, avatar_age, avatar_gender, avatar_id, n)

av_agrl[is.na(av_agrl$n), 5] = 0


mlr <- lm(data = av_agrl[,-1], n~.)
summary(mlr)

###############
#hierarchical clustering
av_agr_p <- av_agr[,-1]

dendrogram <- hclust(dist(av_agr_p, method = 'euclidean'), 
                     method = 'ward.D')
plot(dendrogram,
     main = "Dendrogram",
     xlab = 'Player',
     ylab = 'Euclidean distances')

# determined 4 clusters

# Fitting hierarchical clustering to the mall dataset
hc <- hclust(dist(av_agr_p, method = 'euclidean'), 
             method = 'ward.D')
y_hc <- cutree(hc, k = 4)

# Visualising the clusters
library(cluster)
clusplot(av_agr_p,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of players'))
#xlab = 'Annual Income',
#ylab = 'Spending Score')



