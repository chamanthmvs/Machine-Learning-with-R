#C5.0 Decision Trees 
#Decision Trees are used to make decisions or classifying the data or predicting the data by dividing 
# -the data into small subsets and also dividing the data into tree like structure 

#Identifying risky bank loans 

# Collecting or reading the data

risky_bank_loans <- read.csv("C://Machine-Learning-with-R-datasets-master/credit.csv")

risky_bank_loans

# Data-preprocessing

#identifying the structure of the data
str(risky_bank_loans)

#understanding of the data
table(risky_bank_loans$checking_balance)

table(risky_bank_loans$savings_balance)

#the default column or feature will tell you whether the loan is defaulted or not . 
#So, making it as labeled data. It is not required for all problems . 
#some problems which are not fairly modeled

risky_bank_loans$default<-as.factor(risky_bank_loans$default)

risky_bank_loans$default<-ifelse(risky_bank_loans$default == "1","no","yes") #this will return char type

#so again converting into factor type

risky_bank_loans$default<-as.factor(risky_bank_loans$default)
str(risky_bank_loans)

#checking the type of data again

table(risky_bank_loans$default)

# Training the model

#we need to create a train data as well as test data

#Here we will use random sampling technique
#set.seed() -- which will make the random generator same if we re-execute the program
#if it is not used every time random samples are selected and outputs will differ
#which will create a problem in analyzing the data
set.seed(123)
train_sample <- sample(1000,900)
# train_sample <- sample(2,nrow(risky_bank_loans),prob = c(0.7,0.3),replace = T)

risky_bank_loans_training_data <- risky_bank_loans[train_sample,]

risky_bank_loans_testing_data <- risky_bank_loans[-train_sample,]
#check whether both the samples are similarly sampled -- which should be closer to actual percentage of origial one

table(risky_bank_loans_training_data$default)

#to check the proportion
prop.table(table(risky_bank_loans_training_data$default))

prop.table(table(risky_bank_loans_testing_data$default))


#training the model

#C5.0 will be in C5.0 library

library(C50)
#Here using [-17] because except 17th column which is default every other column will be passed to model
Ml_model <- C5.0(x = risky_bank_loans_training_data[-17],y = risky_bank_loans_training_data$default)

Ml_model

summary(Ml_model)

# plot(Ml_model)
# Evaluating the model
#predict() is used to apply test_dataset to decision tree
ncol(risky_bank_loans_testing_data)

risky_bank_loans_testing_data1 <- risky_bank_loans_testing_data[-17]

ncol(risky_bank_loans_testing_data1)

risky_bank_loans_predict <- predict(Ml_model,newdata = risky_bank_loans_testing_data1)

table(risky_bank_loans_predict)


# CrossTable is to cross verify the results as well as this table takes the parameters of predicted values as well as testdata values 
CrossTable(x = risky_bank_loans_testing_data$default,y = risky_bank_loans_predict,prop.chisq = FALSE)

# Improving the model

#Improving the performance of the model can be done by boosting method

Ml_model_boost <- C5.0(x = risky_bank_loans_training_data[-17],y = risky_bank_loans_training_data$default,trials = 10)
#Trail is a boosting iterations --- it is almost similar to boosting technique which is often used in ensemble learning.
summary(Ml_model_boost)
risky_bank_loans_predict_boost <- predict(Ml_model_boost,risky_bank_loans_testing_data1)
summary(risky_bank_loans_predict_boost)

CrossTable(x = risky_bank_loans_predict_boost,y = risky_bank_loans_testing_data$default,prop.chisq = FALSE)

