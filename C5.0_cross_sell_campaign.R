#creating a decision tree for cross_sell_campaign 

#Just for practice and know the difference in result from rf result to this model result

# Collect the data & read the data
crossSellCampaign <- read.csv("C://COMPUTER/E drive/MACHINE LEARNING/MACHINE LEARNING - CDAC/Machine Learning/Examples/Classification/bank-5000.csv",header = TRUE,sep = ";")

nrow(crossSellCampaign)
# data preparation & data preprocessing
table(crossSellCampaign$y)
str(crossSellCampaign)

# training the model

#divide the data for train as well as test.
#as data is not randomly distributed we use random sampling

smple <- sample(2,nrow(crossSellCampaign),prob = c(0.7,0.3),replace = T) #here the sample is taken with replacement

crossSellCampaign_train <- crossSellCampaign[smple == 1,]
crossSellCampaign_test <- crossSellCampaign[smple == 2,]

decisionTree_model <- C5.0(crossSellCampaign_train[-c(9,11,16,17)],crossSellCampaign_train$y)
decisionTree_model
summary(decisionTree_model)

# evaluating the model
crossSellCampaign_predict <- predict(decisionTree_model,newdata = crossSellCampaign_test[-17])
crossSellCampaign_predict
summary(crossSellCampaign_predict)

CrossTable(crossSellCampaign_predict,crossSellCampaign_test$y)
# improving the performance of the model

decisionTree_model_boost <- C5.0(crossSellCampaign_train[-c(9,11,16,17)],crossSellCampaign_train$y,trials = 10)
decisionTree_model_boost
summary(decisionTree_model_boost)


crossSellCampaign_predict_boost <- predict(decisionTree_model_boost,newdata = crossSellCampaign_test[-17])
crossSellCampaign_predict_boost
summary(crossSellCampaign_predict_boost)

CrossTable(crossSellCampaign_predict_boost,crossSellCampaign_test$y)

