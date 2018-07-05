#This is clustering algorithm where you will find similar group of type at one place and arrange similar kind at one place
#K-means Clustering algorithm

#This is database of US college teens -- find their interest in market segments and making them into different groups
#sns data - social networking service
# Collecting the data

sns_data <- read.csv("C://Machine-Learning-with-R-datasets-master/snsdata.csv")

# Data preparation (Data preprocessing)

# Here we should find the people who have interests towards particular things (like sports,fashion etc...)
# for marketing people to recommend them (this is not bothered). All this data is collected from social networking site

#Understand the data and preprocess data before mining the data

str(sns_data)
# attach(sns_data)
#Analyze the gradYear, age, gender and prepocess the data accordingly
table(sns_data$gender,useNA = "ifany")

# gender has noise values values preprocess them
sns_data$gender <- ifelse(sns_data$gender == "F" & !is.na(sns_data$gender),1,0)
table(sns_data$gender)
# sns_data$females <- ifelse(sns_data$gender == "F" & !is.na(sns_data$gender),1,0)
# table(sns_data$females)
table(sns_data$gender)
#----------------------
summary(sns_data$age)
# Age has noise values , we should preprocess it according to gradyear because we couldn't 
# blindly calculate mean and replace it in the age because 1st gradyear students' age will be lower than 
# final gradyear students' age.
sns_data$age <- ifelse(sns_data$age >= 13 & sns_data$age <=20,sns_data$age,NA)
# There is function called aggregate and ave where we could perform such operations
ave_age <- ave(sns_data$age,sns_data$gradyear, FUN = function(x) mean(x,na.rm = TRUE))
#The below function does is if incase function is true, then it will replace
sns_data$age <- ifelse(is.na(sns_data$age),ave_age,sns_data$age)
summary(sns_data$age)
#Age has noise values so mine them 
# Training the model
#We have to train the model
#Scaling the necessary parameters from the data
ncol(sns_data)
sns_data_filtered <- sns_data[5:40]
# sns_data_filtered_scaled <- scale(sns_data_filtered)
sns_data_filtered_scaled <- scale(sns_data_filtered,center = TRUE)
head(sns_data_filtered_scaled)
sns_data_filtered_scaled <- as.data.frame(sns_data_filtered_scaled)

# As this is clustering there is no meaning in dividing into testing as well as training data
set.seed(12345)
sns_data_model <- kmeans(sns_data_filtered_scaled,5)
sns_data_model
sns_data_model$centers
summary(sns_data_model)
#Visualizing the cluster

library(factoextra)

fviz_cluster(sns_data_model,data = sns_data)

# Evaluating the model
library(ggplot2)
sns_data_model$size
# Improving the performance of the model
#calculating elbow method
# fviz_nbclust(sns_data_filtered_scaled, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)
#computing the distances

# dist_euclidian <- dist(sns_data_filtered_scaled,method = "euclidean")
# as.matrix(dist_euclidian)
# As the size of the matrix is large, it is not able to display the results
#calculating correlation distance
# dist_cor <- get_dist(sns_data_filtered_scaled,method = "pearson")

