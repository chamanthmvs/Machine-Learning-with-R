#This is a dataset about ionosphere and its properties. If incase, if the signal is good
#then it is considered as good RADAR or it is considered as bad RADAR.

library(mlbench)
library(caret)
library(caretEnsemble)

data("Ionosphere")

ionosphereData <- Ionosphere
typeof(ionosphereData)
class(ionosphereData)

ionosphereData[1:5,]

str(ionosphereData)

ionosphereData$V1 <- as.numeric(ionosphereData$V1)

ionosphereData$V2 <- as.numeric(ionosphereData$V2)

str(ionosphereData)

c5.o_model <- train(ionosphereData$Class~. , data = ionosphereData, method = "C5.0" , metric = "Accuracy" , trControl = trainControl(method = "cv",number = 10,repeats = 3))
#This is Gradient Boost model
GBM_model <- train(ionosphereData$Class~., data = ionosphereData, method = "gbm", metric = "Accuracy", trControl = trainControl(method = "cv",number = 10 ,repeats = 3))

boost_results <- resamples(list(c5.o_model,GBM_model))
summary(boost_results)
dotplot(boost_results)

