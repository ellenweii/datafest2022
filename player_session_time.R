

#############################################
library(tidyverse)
#main_data <- read_csv("logs.csv", guess_max = 2106600)
main_data <- full_data

category <- unique(main_data$event_category)
last <- main_data$event_category[1]
time <- main_data$event_time_dbl[1]
df<-c()

for (i in 2:length(main_data$event_category)) {
  current<- main_data$event_category[i]
  if(current!=last){
    df<-rbind(df,c(main_data$session[i-1], main_data$player_id[i-1] ,last, time))
  }
  last <- current
  time <- main_data$event_time_dbl[i]
}
df
dfdf <- data.frame(session = df[,1], playerID = df[,2], category = df[,3], cumu_time=as.numeric(df[,4]))
dfdf

total_player <-dfdf %>% 
  group_by(playerID) %>% 
  count()
total_player$playerID


individual<-function(d_f){
  category <- unique(d_f$category)
  last <- d_f$category[1]
  time <- d_f$cumu_time[1]
  v<- c()
  df<-c()
  for (i in 2:length(d_f$category)) {
    current<- d_f$category[i]
    if(current!=last){
      df<-rbind(df,c(d_f$session[1] ,d_f$playerID[1] ,last, time))
    }
    last <- current
    time <- d_f$cumu_time[i]
  }
  #df<-data.frame(playerID = df[,1], category = df[,2], cumu_time=as.numeric(df[,3]))
  df <- data_frame(session = df[,1], playerID = df[,2],category = df[,3], time_spend=diff(c(0,as.numeric(df[,4]))))
  output<-df %>% 
    group_by(session, category) %>% 
    summarise(sum = sum(time_spend))
  
  #output <- df %>% 
   # group_by(playerID, category) %>% 
    #summarise(time_spend = diff(c(0,df[,3])))
  cbind(ID=df$playerID[1],output)
}


a <-c()
#begin <- individual(dfdf[dfdf$playerID==total_player$playerID[1],])
for(i in 1:length(total_player$playerID)){
  input <- individual(dfdf[dfdf$playerID==total_player$playerID[i],])
  a<-rbind(a, input)
}
a # 166 player's time-spend on each event category 

answer <- a %>% group_by(gsub(" ", "", session),category) %>% 
  summarise(total=sum(sum)) %>% 
  arrange(desc(total))
View(answer)

format<-str_detect(answer$`gsub(" ", "", session)`,"Session\\d\\b")
answer2 <- answer[format,] %>% arrange(`gsub(" ", "", session)`)


#########
df <- read_csv("player_session_time.csv")

dfc <- df[,-1]
#av_agr_p[is.na(av_agr_p)] <- 0

dendrogram <- hclust(dist(dfc, method = 'euclidean'), 
                     method = 'ward.D')
plot(dendrogram,
     main = "Dendrogram",
     xlab = 'Player',
     ylab = 'Euclidean distances')

# determined 6 clusters

# Fitting hierarchical clustering to the mall dataset
hc <- hclust(dist(dfc, method = 'euclidean'), 
             method = 'ward.D')
y_hc1 <- cutree(hc, k = 6)

# Visualising the clusters
library(cluster)
clusplot(dfc,
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

label_players <- cbind(df[,1], y_hc1)

write.csv(label_players,"player_session_labels.csv", row.names = FALSE)

unique(df$`gsub(" ", "", session)`)
