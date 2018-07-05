# Build a predictive model which will identify customers who are more likely to 
# respond to term deposit cross sell campaign.

#we are using random forest model to make better predictions when compared to decision trees, it can make better predictions moreover it works best for large amount of data.

#This is a dataset containing all the details of customers of bank

library(randomForest)

cross_sell_campaign <- read.csv("C://COMPUTER/E drive/MACHINE LEARNING/MACHINE LEARNING - CDAC/Machine Learning/Examples/Classification/bank-5000.csv",header = TRUE,sep = ";")


str(cross_sell_campaign)

#column names of the campaign

names(cross_sell_campaign)

#sampling the data 
set.seed(123)

smple <- sample(2,nrow(cross_sell_campaign),prob = c(0.7,0.3),replace = TRUE)
cross_sell_campaign_train <- cross_sell_campaign[smple == 1,]
cross_sell_campaign_test <- cross_sell_campaign[smple == 2,]

prop.table(table(cross_sell_campaign_test$default))

prop.table(table(cross_sell_campaign_train$default))

#applying the model 

# before applying the model plz make a note that randomForest model requires a formula

# Eliminating the target variable from the predictor variable

cross_sell_campaign_train1 <- cross_sell_campaign_train[-17]
cross_sell_campaign_test1 <- cross_sell_campaign_test[-17]

ML.model <- randomForest(cross_sell_campaign_train$y~.,data = cross_sell_campaign_train1)
ML.model

importance(ML.model)

# This will help in knowing which feature has most importance among all
varImpPlot(x = ML.model,sort = TRUE)

#Evaluating the model

cross_sell_campaign_predict <- predict(ML.model,newdata = cross_sell_campaign_test1)
cross_sell_campaign_predict
summary(cross_sell_campaign_predict)

CrossTable(cross_sell_campaign_predict,cross_sell_campaign_test$y)

