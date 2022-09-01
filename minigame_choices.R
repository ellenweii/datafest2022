#mini game avatar choices

#Player hits save button on aspirational avatar question popup

avatar <- data %>% 
  filter(event_id == 304) %>% 
  select(row_id, event_time_dbl, data_headers, data_values)

#t <- unlist(strsplit(x = avatar[[1,4]], split = ','))
#t[6]


choice_6 <- c()
for (i in 1:nrow(avatar)){
  sixth <- unlist(strsplit(x = avatar[[i,4]], split = ','))[6]
  choice_6 <- c(choice_6, sixth)
}

avatar$new_choice_value <- choice_6

choice_id <- c()
for (i in 1:nrow(avatar)){
  chunk <- unlist(strsplit(x = avatar[[i,4]], split = ','))[1:3]
  chunk <- paste(chunk, collapse="")
  choice_id <- c(choice_id, chunk)
}

avatar$new_choice_id <- choice_id

avatar_choices <- avatar %>% 
  select(new_choice_id, new_choice_value) %>% 
  group_by(new_choice_id)  %>% do(tail(., n=1)) %>% 
  pivot_wider(names_from = new_choice_id, values_from = new_choice_value)

avatar_choices <- cbind(avatar$row_id[1], avatar_choices)
avatar_choices <- avatar_choices %>% 
  mutate("101_011" = paste(sort(c(`010`, `011`)), collapse = " "),
         "110_111_120_121" = paste(sort(c(`110`, `111`, `120`, `121`)), collapse =" "),
         "220_230" = paste(sort(c(`220`, `230`)), collapse =" "),
         "520_521" = paste(sort(c(`520`, `521`)), collapse =" "),
         "600_612" = paste(sort(c(`600`, `601`, `602`, `610`, `611`, `612`)), collapse =" "),
         "700_721" = paste(sort(c(`700`, `701`, `702`, `711`, `712`, `720`, `721`)), collapse =" ")
         ) %>% 
  select(-c(`010`, `011`, `110`, `111`, `120`, `121`, `220`, `230`, `520`, `521`,
            `600`, `601`, `602`, `610`, `611`, `612`, `700`, `701`, `702`, `711`,
            `712`, `720`, `721`))



### apply to full_data ###############
#########################################
library(tidyverse)
library(dplyr)
library(gtools)

full_data <- read_csv("logs.csv")

player_ids <- unique(full_data$player_id)

avatar_all <- as.data.frame(matrix(nrow = 1, ncol=64))
colnames(avatar_all) <- colnames(avatar_choices)

for (player in player_ids){
  #subset for a single player
  avatar <- full_data %>% 
    filter(player_id == player, event_id == 304) %>% 
    select(row_id, event_time_dbl, data_headers, data_values)
  
  #choose 6th value in choice vector
  choice_6 <- c()
  for (i in 1:nrow(avatar)){
    sixth <- unlist(strsplit(x = avatar[[i,4]], split = ','))[6]
    choice_6 <- c(choice_6, sixth)
  }
  
  avatar$new_choice_value <- choice_6
  
  #code in choice id
  choice_id <- c()
  for (i in 1:nrow(avatar)){
    chunk <- unlist(strsplit(x = avatar[[i,4]], split = ','))[1:3]
    chunk <- paste(chunk, collapse="")
    choice_id <- c(choice_id, chunk)
  }
  
  avatar$new_choice_id <- choice_id
  
  #pivot into 1 row
  avatar_choices <- avatar %>% 
    select(new_choice_id, new_choice_value) %>% 
    group_by(new_choice_id)  %>% do(tail(., n=1)) %>% 
    pivot_wider(names_from = new_choice_id, values_from = new_choice_value)
  
  avatar_choices <- cbind(player, avatar_choices)
  
  cat("number of columns:", ncol(avatar_choices), "\n")
  
  #if (ncol(avatar_choices) == 64){
  #  avatar_all <- rbind(avatar_all, avatar_choices)
  #}
  
  #View(avatar_all)
  avatar_all <- smartbind(avatar_all, avatar_choices)
    
}

avatar_all <- avatar_all[-1,]


##############clustering ###########
#hierarchical clustering
av_agr_p <- avatar_all[,-1]
av_agr_p[is.na(av_agr_p)] <- 0

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


###########
avatar_all <- cbind(y_hc, avatar_all)



colnames(avatar_all)
labeled_player <- avatar_all[, c("y_hc", "player")]

write.csv(labeled_player,"labeled_player.csv", row.names = FALSE)



