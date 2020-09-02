###################----- NLP  -----##########################
getwd()
library(doParallel)
registerDoParallel(cores = 4)
# uploading the data
library(readr)
train <- read.csv(choose.files()) #choose training data
test <- read.csv(choose.files()) #choose testing data

# adding the target to test data
test$target <- NA

# differnce bet train and test
test$istrain <- FALSE
train$istrain <- TRUE
# combining both train and test
data.all <- rbind(train,test)

# Converting factor into char
#data.all$keyword <- as.character(data.all$keyword)
#data.all$location <- as.character(data.all$location)
data.all$text <- as.character(data.all$text)

# na in test data
table(is.na(data.all)) 
summary(data.all)

#######---  NLP  ---######
#install.packages("tm")
library(tm)

target.corpus <- Corpus(VectorSource(data.all$text))

# preparing a clean corpus
removeURL <-  function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunc<-function(x)  gsub("[^[:alpha:][:space:]]*","",x)
removeUsername <- function(x) gsub("@[^[:space:]]*", "", x)
removeSingle <- function(x) gsub(" . ", " ", x)

corpus_clean <- tm_map(target.corpus, content_transformer(removeURL))
corpus_clean <- tm_map(corpus_clean,content_transformer(removeNumPunc))
corpus_clean <- tm_map(corpus_clean, content_transformer(removeUsername))
corpus_clean <- tm_map(corpus_clean, content_transformer(removeSingle))

corpus_clean <- tm_map(corpus_clean,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean, removeWords,myStopWords <- c((stopwords('english')), c("really", "tweets", "saw", "just", "feel", "may", "us", "rt", "every", "one",
                                                                                          "amp", "like", "will", "got", "new", "can", "still", "back", "top", "much",
                                                                                          "near", "im", "see", "via", "get", "now", "come", "oil", "let", "god", "want",
                                                                                          "pm", "last", "hope", "since", "everyone", "food", "content", "always", "th",
                                                                                          "full", "found", "dont", "look", "cant", "mh", "lol", "set", "old", "service",
                                                                                          "city", "home", "live", "night", "news", "say", "video", "people", "ill", 
                                                                                          "way",  "please", "years", "take", "homes", "read", "man", "next", "cross", 
                                                                                          "boy", "bad", "ass")))
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)

corpus_clean <- stemDocument(corpus_clean)
inspect(corpus_clean[400])

# after cleaning the corpus we will create DTM
corpus_dtm <- DocumentTermMatrix(corpus_clean)

# creating TDM
?TermDocumentMatrix()
corpus_tdm <- TermDocumentMatrix(corpus_clean)
is.na(corpus_tdm)

# Checking both TDM & DTM
View(corpus_dtm)
View(corpus_tdm)
#number of documnets
corpus_tdm$nrow

#number of words after cleaning the raw corpus
corpus_tdm$ncol

# checking for empty index
a0 <- NULL
a1 <- NULL

# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(corpus_dtm))
{ if (sum(corpus_dtm[, i1]) == 0){a0 = c(a0, i1)} }
for (i1 in 1:ncol(corpus_tdm))
{ if (sum(corpus_tdm[, i1]) == 0) {a1 = c(a1, i1)} }

a0
a1 # 7627

# Fixing incomplete cases
colTotals <- apply(corpus_tdm, 2, sum) #Find the sum of words in each Document

corpus_tdm1 <- corpus_tdm[,colTotals>0]      

a3 = NULL
for (i1 in 1:ncol(corpus_tdm1))
{ if (sum(corpus_tdm1[, i1]) == 0) {a3 = c(a3, i1)} }

#incomplete.cases <- which(!complete.cases(corpus_tdm))
#corpus_tdm[incomplete.cases,] <- rep(0.0, ncol(corpus_tdm))

#prepairing train and test data
train_raw <- data.all[data.all$istrain==TRUE,]      
#we had split the original data to get the classification column because its not 
#there in the dtm or cleaned corpus
test_raw <- data.all[data.all$istrain==FALSE,]

table(is.na(train_raw))

# splitting into train and val
train_raw$target <- as.factor(train_raw$target)
train.new <- train_raw[1:6090,]
val.raw <- train_raw[6091:7613,]

# checking the proptional table
prop.table(table(train_raw$target))
prop.table(table(train.new$target))
prop.table(table(val.raw$target))

# splitting the corpus dtm
train_dtm <- corpus_dtm[1:6090,]
val_dtm <- corpus_dtm[6091:7613,] 
test_dtm<-corpus_dtm[7614:10876,]

# splitting the clean corpus
train_corpus <- corpus_clean[1:6090]
val_corpus <- corpus_clean[6090:7613]
test_corpus<-corpus_clean[7614:10876]

# converting keyword into keyword
data.all$keyword <- as.factor(data.all$keyword)
levels(data.all$keyword)

# creating a dictory 
target_dict <- findFreqTerms(train_dtm,3)
list(target_dict[1:50])

# creating a new dtm from the cleaned corpus with the words in dict words
target_train <- DocumentTermMatrix(train_corpus,list(dictionary=target_dict))
target_val <- DocumentTermMatrix(val_corpus,list(disctionary=target_dict))

test_dtm <- DocumentTermMatrix(test_corpus,list(disctionary=target_dict))

#writing a function which checks if word is there in the dtm then 1 else 0 frequency value doent matter and converting them into factors, giving them labels also. 
convert_count<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c("NO","YES"))
}

# applying the convert_count function to get final sms_train and sms_test
target_train <- apply(target_train,MARGIN=2, convert_count)
target_val <- apply(target_val,MARGIN=2,convert_count)

test <- apply(test_dtm, MARGIN = 2, convert_count)

#####################---   Naive Bayes  Model   ---###########################
library(e1071)
?naiveBayes
NB.model <- naiveBayes(target_train, train.new$target)
NB.model$levels
# prediction on trian data
pred_train <- predict(NB.model, newdata = target_train)

# accuracy for train data
train.acc <- mean(pred_train==train.new$target)*100
train.acc
# 57.93
# 71.36
# 83.84
# prediction on val data
pred_val <- predict(NB.model, newdata = target_val)

# accuracy for val data
val.acc <- mean(pred_val==val.raw$target)*100
val.acc
#64%
# 66%
# predicting on test data
pred_test <- predict(NB.model, newdata = test)

# Saving the prediction and writing the csv format
id <- test_raw$id
output <- as.data.frame(id)
output$target <- as.factor(pred_test) 
table(output$target)
View(output)

setwd("C:/Users/Adarsh Sambare/Documents/GitHub/NLP-Kaggle")
getwd()
write.csv(output, file = "kaggle.submission.NB2.csv", row.names = F)
