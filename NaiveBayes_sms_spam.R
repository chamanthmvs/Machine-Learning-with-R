# Classification algorithm
# 
# NaiveBayes classification algorithm.  This is used mainly for conditional probability kind of filtering and classifying objects
# 
# In this example we are going to classify phone messages from ham--legitimate messages  and spam--spam or false messages 

#Collecting the data

# read the data from the CSV file

sms_data <- read.csv("C://Machine-Learning-with-R-datasets-master/sms_spam.csv")

str(sms_data)

# this is a text file so it requires a lot of text processing
#Data preprocessing


table(sms_data$type)

sms_data_text <- sms_data$text

sms_data_text[1:3]

# as it is text we need to make a corpus before preprocessing the data
# 
# we are performing text mining . So, install tm (textmining) package to proceed further
# 
library(tm)

#readerControl() function is used to read the words from PDF,Word Documents etc....

# here as we already have the data. So, we need not to use readerControl 
# we will use VectorSource and VCorpus function from tm package
# VCorpus is used to convert entire text into a Corpus.
# Corpus is nothing but a collection of textDocuments
# VectorSource is one of the function similar to readerControl

sms_corpus <- VCorpus(VectorSource(sms_data_text))

sms_corpus

# It is a list . To view inside the sms_copus just use inspet() . where you can see the items inside it

inspect(sms_corpus[1:5])

#to view the contents convert it into characters
#use lapply() to apply the function to whole corpus

lapply(sms_corpus,as.character)

sms_corpus[1:3]

# Now perform preprocessing of text such as converting the text to lower, removingNumbers as well as stopWords etc....

# Use DocumentTermMatrix which is also a part of the tm package
# 
# For stemming use snowball package
library(SnowballC)
sms_dtm <- DocumentTermMatrix(sms_corpus,control = list(stopwords = TRUE,removeNumbers = TRUE,removePunctuation = TRUE,tolower = TRUE,stemming = TRUE))

# Create Train and Test sets

sms_train <- sms_dtm[1:4274,]
sms_test <- sms_dtm[4275:5574,]

#Storing the labels of test and train

sms_train_label <- sms_data[1:4274,]$type

sms_test_label <- sms_data[4275:5574,]$type

#---------Visualization-----------------
#WordCloud to visualize the words
library(wordcloud)
#These are all words in the sms_corpus
wordcloud(sms_corpus,max.words = 100,min.freq = 5)
#Words in the actual spam messages
sms_ham <- subset(sms_data,type == "ham")
wordcloud(sms_ham$text,max.words = 75)
#Words in the actual spam messages
sms_spam <- subset(sms_data,type == "spam")
wordcloud(sms_ham$text,max.words = 75)

#Training the model
#Taking the words whose frequency is greater than 5
sms_freq <- findFreqTerms(sms_train,5)
sms_freq_train <- sms_train[,sms_freq]
sms_freq_test <- sms_test[,sms_freq]
# This model will accept the input in binary values like yes or no. So, convert
convert_count <- function(x){
  x <- ifelse(x>0,"yes","no")
}

sms_train_filtered <- apply(sms_freq_train, MARGIN = 2,convert_count)
sms_test_filtered <- apply(sms_freq_test, MARGIN = 2,convert_count)

library(e1071)
naive_bayes_model <- naiveBayes(sms_train_filtered,sms_train_label)
naive_bayes_model
#Evaluating the model
naive_bayes_model_predict <- predict(naive_bayes_model,sms_test_filtered)

library(gmodels)
CrossTable(naive_bayes_model_predict,sms_test_label,prop.chisq = FALSE)
#Improving the performance of the model

# Use laplace to minimize the error and maximize the accuracy

naive_bayes_model2 <- naiveBayes(sms_train_filtered,sms_train_label,laplace = 1)

naive_bayes_model2_predict <- predict(naive_bayes_model2,sms_test_filtered)

CrossTable(naive_bayes_model2_predict,sms_test_label)

