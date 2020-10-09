setwd("C:\\Users\\user\\Desktop\\data analytics\\20.04.19_naive")
review_raw <- read.csv("0_Life Insurance.csv")
#names(sms_raw)=c("SlNo","type","text")

str(review_raw)
table(review_raw$Rating)
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
      library(caret)
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
      
      