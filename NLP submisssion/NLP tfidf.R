###############---   TFIDF  ---####################
library(tm)

# creating a corpus
target.corpus.tdm.tdm <- Corpus(VectorSource(data.all$text))
target.corpus.tdm.tdm$content[1:20]

# cleaning the corpus

# lOADING +VE AND -VE words  
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words) 			# including our own positive words to the existing list
neg.words = c(neg.words)
stopwdrds = readLines(file.choose())

# preparing a clean corpus

# function for removing unecessary items
removeNumPunc<-function(x)  gsub("[^[:alpha:][:space:]]*","",x)
removeURL <-  function(x) gsub("http[^[:space:]]*", "", x)
removeUsername <- function(x) gsub("@[^[:space:]]*", "", x)
removeSingle <- function(x) gsub(" . ", " ", x)
corpus_clean.tdm <- tm_map(target.corpus.tdm, content_transformer(removeSingle))
corpus_clean.tdm <- tm_map(target.corpus.tdm, content_transformer(removeUsername))
corpus_clean.tdm <- tm_map(target.corpus.tdm, content_transformer(removeURL))
corpus_clean.tdm <- tm_map(target.corpus.tdm,content_transformer(removeNumPunc))

corpus_clean.tdm<-tm_map(target.corpus.tdm,removeNumbers)
corpus_clean.tdm<-tm_map(target.corpus.tdm, removeWords,stopwords())
corpus_clean.tdm<-tm_map(target.corpus.tdm,removePunctuation)
corpus_clean.tdm <- tm_map(target.corpus.tdm,stripWhitespace)
corpus_clean.tdm <- tm_map(target.corpus.tdm,tolower)
corpus_clean.tdm$content[1:10]

# TDM 
# Term document frequency matrix
data.all.tdm <- TermDocumentMatrix(corpus_clean.tdm)

# Term document matrix with inverse frequency 
data.all.tdm1 <- TermDocumentMatrix(target.corpus.tdm,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))#,stemming=T))
inspect(data.all.tdm1)

# checking for empty index
a0 <- NULL
a1 <- NULL
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(data.all.tdm))
{ if (sum(data.all.tdm[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(data.all.tdm1))
{ if (sum(data.all.tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

a0 #null
a1 #null
# no need to Remove empty docs 

# Document term matrix 
data.all.dtm3 <- t(data.all.tdm)
data.all.dtm4 <- t(data.all.tdm1)
corpus_clean.dtm <- t(corpus_clean.tdm)

# splitting into train and val
train_raw$target <- as.factor(train_raw$target)
train.new <- train[1:6090,]
val.raw <- train[6091:7613,]

# splitting the corpus
train_dtm <- corpus_clean.dtm[1:6090,]
val_dtm <- corpus_clean.dtm[6091:7613,] 
test_dtm<-corpus_clean.dtm[7614:10876,]

# splitting the clean corpus
train_corpus <- corpus_clean[1:6090]
val_corpus <- corpus_clean[6090:7613]
test_corpus<-corpus_clean[7614:10876]

# converting keyword into keyword
data.all$keyword <- as.factor(data.all$keyword)
levels(data.all$keyword)

# creating a dictory 
target_dict <- findFreqTerms(corpus_dtm,4)