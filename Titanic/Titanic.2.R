####################### TITANIC WITH SOFT CODING ###########################
#Reading the data
library(readr)
titanic_train <- read.csv(choose.files()) #choose training data
titanic_test <- read.csv(choose.files()) #choose testing data

attach(titanic_train)
attach(titanic_test)
#viewing the data
View(titanic_train)
View(titanic_test) 
################################ EDA #################################

# creating survived col in test
titanic_test$Survived <- NA

# creating new col for train set
titanic_train$Istrain <- TRUE
titanic_test$Istrain <- FALSE
# combining them into one data
titanic_all <- rbind(titanic_test,titanic_train)

# removing unnecessary columns
titanic_all <- titanic_all[-c(1,3,8,10)]

#checking the null values
table(is.na(titanic_all)) ## there are 682 null values
# id no is just serial no and names 

# checking the summary
summary(titanic_all) ## age column is having the null values

# cabin and embarked are having missing values as "" 
#table(titanic_all$Cabin)
#levels(titanic_all$Cabin)[1] = "missing"


# imputing na With regression
hist(titanic_all$Age, horizontal = T)
boxplot(titanic_all$Age, horizontal = T)

# checking the uppar limit of box plot
boxplot.stats(titanic_all$Age)$stats

# creating a filter for age
upper.limit <- boxplot.stats(titanic_all$Age)$stats[5]
lower.limit <- boxplot.stats(titanic_all$Age)$stats[1]

age.filter <- titanic_all$Age < upper.limit
table(age.filter)
age.filter2 <- lower.limit > age.filter
table(age.filter2)
# age less than upper limit
titanic_all[age.filter2,]

# regression model for predicting age
age.pred <- lm(Age~ Pclass+Sex+Fare+SibSp+Parch+Embarked,
                data =titanic_all[age.filter,])


age.miss <- titanic_all[is.na(titanic_all$Age),]

# predicting for NA in age
age.prediction <- predict(age.pred, newdata = age.miss)

# some negative values
age.prediction[age.prediction < 0] = 0.17

# replacing the NA with predicted values
titanic_all[is.na(titanic_all$Age), "Age"] <- age.prediction
table(is.na(titanic_all$Age))

# replacing fare value with logical no
titanic_all[is.na(titanic_all$Fare), "Fare"] <- 8.253221

#################    NOW   XXXXXXXXXX    EDA    #####################

