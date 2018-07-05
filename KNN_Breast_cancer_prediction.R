# This is a KNN- Algorithm example which is one of the classification algorithm 
# which is easy one as well as a very powerful algorithm
# 
# Steps in building ML models
# 
# 1. Collecting the data

#reading the data from the CSV file

breast_cancer_data <- read.csv("C://Machine-Learning-with-R-datasets-master/wisc_bc_data.csv")
View(breast_cancer_data)

#Preprocess the data for ML task

# 2. Data preparation (data preprocessing)

#To understand the structure of the data
str(breast_cancer_data)

#All these features or parameters are related to cells in the lump which is taken from the breast. 
#Need not to understand the data as it is a labeled data.

#id column is of no use because it is there only to give you the unique number or indications of records or Observations
#So removing the id column or feature

breast_cancer_data <- breast_cancer_data[-1] #will return the dataframe except first column

str(breast_cancer_data)

#In KNN - the labels are the most important one as KNN does not do anything in the background
#it only relies on the labels of the trained data

#Here diagnosis feature gives us class (which is insight of data -either benign or malignant)

#assign the labels to those B and M

breast_cancer_data$diagnosis <- factor(x = breast_cancer_data$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))

table(breast_cancer_data$diagnosis)
#again check the data
View(breast_cancer_data)

#Normalize the data in order to bring the all the features into a equal scale or else one scale will 
#have a huge effect on the other scale

#there are two types of scaling or normalizing --- Z-score and Normalization

#Normalization is traditional model for KNN, but test with others two
normalize <- function(x){
  return((x-min(x)/max(x)-min(x)))
}

#using lapply() function because it will apply the function to each and every element in the list
#it will return list so converting it into dataFrame

breast_cancer_data_normalized <- as.data.frame(lapply(breast_cancer_data[,2:31],normalize))

breast_cancer_data_normalized
# 3. Training the model

#we need to divide the data into test and train 
#In this case the data is already distributed randomly so we are directly taking samples orelse
#perform sampling is the option

breast_cancer_data_normalized_train <- breast_cancer_data_normalized[1:469,]
breast_cancer_data_normalized_test <- breast_cancer_data_normalized[470:569,]

#Storing labels of these data in different variables
breast_cancer_data_normalized_train_label <- breast_cancer_data[1:469,1]
breast_cancer_data_normalized_test_label <- breast_cancer_data[470:569,1]

#from the library class call the knn model

library(class)

breast_cancer_data_prediction <- knn(train = breast_cancer_data_normalized_train,test = breast_cancer_data_normalized_test,cl = breast_cancer_data_normalized_train_label,k = 21)

#here we took 21 as it is better to go for the squareroot of the number of observations . 
#it is not fixed, better to change values of k and try

# 4. Evaluating the model

#we will use cross table

library(gmodels)

CrossTable(x = breast_cancer_data_prediction,y = breast_cancer_data_normalized_test_label,prop.chisq = FALSE)



# 5. Improving the model

#as we got the 96% we will try further more
#so we are trying to change the scaling type of features

#here we need not to write lapply() or function , there is inbuilt function in r -- i.e., scale()

breast_cancer_data_zScore <- breast_cancer_data[-1]
breast_cancer_data_zScore <- as.data.frame(scale(breast_cancer_data_zScore))
breast_cancer_data_zScore_train <- breast_cancer_data_zScore[1:469,]
breast_cancer_data_zScore_test <- breast_cancer_data_zScore[470:569,]
breast_cancer_data_zScore_test_labels<-breast_cancer_data[470:569,1]
breast_cancer_data_zScore_train_labels<-breast_cancer_data[1:469,1]

breast_cancer_data_zScore_prediction <- knn(train = breast_cancer_data_zScore_train,test = breast_cancer_data_zScore_test,cl =breast_cancer_data_zScore_train_labels, k=21)

CrossTable(x=breast_cancer_data_zScore_test_labels,y=breast_cancer_data_zScore_prediction,prop.chisq = FALSE)

