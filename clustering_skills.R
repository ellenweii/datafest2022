#clustering_skills

#hierarchical clustering
dendrogram <- hclust(dist(sessions, method = 'euclidean'), 
                     method = 'ward.D')
plot(dendrogram,
     main = "Dendrogram",
     xlab = 'Player instance',
     ylab = 'Euclidean distances')

# determined 5 clusters

# Fitting hierarchical clustering to the mall dataset
hc <- hclust(dist(sessions, method = 'euclidean'), 
             method = 'ward.D')
y_hc <- cutree(hc, k = 5)

# Visualising the clusters
library(cluster)
clusplot(sessions,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of player instances'))
         #xlab = 'Annual Income',
         #ylab = 'Spending Score')




