setwd("C:\\Users\\anuru\\Desktop\\Data Ananlytics with R programing\\Day 7")
review_raw <- read.csv("0_Life Insurance.csv")
#names(sms_raw)=c("SlNo","type","text")

str(review_raw)
table(r(review_raw$Rating)
summary(review_raw$Rating)
review_raw$type=ifelse(review_raw$Rating>=4,"positive","negative")
#sms_raw$type <- factor(sms_raw$type)#if not present as factore
#table(review_raw$type)

library(tm)
review_corpus <- Corpus(VectorSource(review_raw$Review_Text))
print(review_corpus)
inspect(review_corpus[1:10])

#pre-processing of text
corpus_clean <- tm_map(review_corpus, content_transformer(tolower))
corpus_clean <- tm_map(review_corpus, removeNumbers)

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
review_dtm <- DocumentTermMatrix(corpus_clean)

index=createDataPartition(review_raw$type, p=0.8,list=F)
review_raw_train <- review_raw[index, ]
review_raw_test <- review_raw[-index, ]

review_dtm_train <- review_dtm[-index,]
review_dtm_train <- review_dtm[-index,]

review_corpus_train <- corpus_clean[index]
review_corpus_test <- corpus_clean[-index]

prop.table(table(review_raw_train$type))
prop.table(table(review_raw_test$type))

library(wordcloud)
wordcloud(review_corpus_train, min.freq = 30)
wordcloud(review_corpus_train, min.freq = 30, random.order = FALSE)


spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5), colors = brewer.pal(3,"Blues"))

findFreqTerms(sms_dtm_train, 5)

sms_dict <- c(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, 2, convert_counts)
sms_test <- apply(sms_test, 2, convert_counts)

summary(sms_train[, 1:5])

## Step 3: Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
names(sms_classifier)

sms_classifier$tables[1:2]

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,
           dnn = c('predicted', 'actual'))
