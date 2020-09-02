#################------  NLP DICTONARY ------########################

# converting keyword into factor
data.all$keyword <- as.factor(data.all$keyword)
levels(data.all$keyword)

# creating a NEW dictonary by keywords
target_dict_new <- levels(data.all$keyword)

# converting the dictonary  
target_dict_new <- as.character(target_dict_new)

# converting it into corpus
dic.corpus <- Corpus(VectorSource(target_dict_new))
dic.corpus$content[1:50]

# cleaning the corpus
library(tm)

#preparing a clean corpus
dic_clean <- tm_map(dic.corpus,tolower)
dic_clean<-tm_map(dic.corpus,removeNumbers)
dic_clean<-tm_map(dic.corpus, removeWords,stopwords())
dic_clean<-tm_map(dic.corpus,removePunctuation)
removeNumPunc<-function(x)  gsub("[^[:alpha:][:space:]]*","","'",x)
dic_clean<-tm_map(dic.corpus,content_transformer(removeNumPunc))
dic_clean<-tm_map(dic.corpus,stripWhitespace)
dic_clean$content[1:10]

dic.new <- dic_clean$content
