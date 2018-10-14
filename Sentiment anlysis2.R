library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)

df<- read.csv("datajokowi.csv", stringsAsFactors = FALSE)
glimpse(df)

set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)

df$class <- as.factor(df$class)

corpus <- Corpus(VectorSource(df$text))

corpus

inspect(corpus[1:3])


dtm <- DocumentTermMatrix(corpus)
inspect(dtm[40:50, 10:15])

df.train <- df[1:3022,]
df.test <- df[3023:3778,]

dtm.train <- dtm[1:3022,]
dtm.test <- dtm[3023:3778,]

corpus.train <- corpus[1:3022]
corpus.test <- corpus[3023:3778]

dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 10)
length((fivefreq))
fivefreq
## [1] 12144

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
## [1]  1500 12144

dtm.test.nb <- DocumentTermMatrix(corpus.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

# Train the classifier
system.time( classifier <- naiveBayes(trainNB, df.train$class, laplace = 1) )

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$class )

# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$class)

conf.mat

conf.mat$byClass

conf.mat$overall
conf.mat$overall['Accuracy']
