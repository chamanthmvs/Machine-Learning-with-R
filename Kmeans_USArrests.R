#Applying k-means clustering for USArrests data

# Collecting the data
# USArrests is inbuilt dataset
head(USArrests)

USArrests_data <- USArrests
typeof(USArrests_data)
class(USArrests_data)
# Data preprocessing
#scale the data 

USArrests_data_scaled <- scale(USArrests_data)
USArrests_data_scaled <- as.data.frame(USArrests_data_scaled)
#Calculating the distance for elbow method

dist_euclidean <- dist(USArrests_data_scaled,method = "euclidean")
round(as.matrix(dist_euclidean),1)

#Finding the correlation among data
library(factoextra)
dist_pearson <- get_dist(USArrests_data_scaled,method = "pearson")
round(as.matrix(dist_pearson),1)

fviz_dist(dist_euclidean)
fviz_dist(dist_pearson)

#using elbow method
fviz_nbclust(x = USArrests_data_scaled,kmeans,method = "wss")+geom_vline(xintercept = 4,linetype = 3)
# Training the model

USArrests_model <- kmeans(x = USArrests_data_scaled,centers = 4,iter.max = 10,nstart = 1)
USArrests_model
#Visualizing the model
fviz_cluster(USArrests_model,USArrests_data_scaled)
# Evaluating the model
USArrests_model$cluster
USArrests_model$size
# Improving the performance of model

