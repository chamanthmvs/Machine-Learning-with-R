#This is a grocery dataset 
#We will use association rule mining algorithm to define rules to make profitable for the company or market
# 
# Collecting the data

library(arules)
grocery_data <- read.transactions("C://Machine-Learning-with-R-datasets-master/groceries.csv",sep = ",")

# Data preprocessing
summary(grocery_data)
inspect(grocery_data[1:5])
itemFrequency(grocery_data[,1:5])

itemFrequencyPlot(grocery_data,support = 0.1)
itemFrequencyPlot(grocery_data,topN = 25)

image(grocery_data[1:100])
# Training the model
aprioriModel <- apriori(grocery_data,parameter = list(support = 0.005,conf = 0.5,minlen = 2))
aprioriModel
summary(aprioriModel)

# Evaluating the model

inspect(aprioriModel)
inspect(aprioriModel[1:5])
inspect(sort(aprioriModel,by = "lift")[1:10])

#selecting a subset from entire list

onionRules <- subset(aprioriModel,items %in% "onions")
summary(onionRules)
inspect(onionRules)

#Storing all these rules into a CSV file

write(x = aprioriModel,file = "GroceryRules.csv",sep = ",")

aprioriModel_dataFrame <- as(aprioriModel,"data.frame")
str(aprioriModel_dataFrame)
