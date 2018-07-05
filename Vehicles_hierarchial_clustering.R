#heirarchial clustering

head(mtcars)

cars_data_frame <- mtcars

#finding the distance 
cars_data_frame_dist <- dist(cars_data_frame,method = "euclidean")

head(cars_data_frame_dist)

library(gmodels)

hierarchial_cluster_model <- hclust(cars_data_frame_dist,method = "complete")

plot(hierarchial_cluster_model)

hierarchial_cluster_model
library(factoextra)
fviz_dend(x = hierarchial_cluster_model,k = 4)
