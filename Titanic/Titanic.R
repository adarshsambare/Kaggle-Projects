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

#checking the null values
table(is.na(titanic_all)) ## there are 682 null values
# id no is just serial no and names 

# checking the summary
summary(titanic_all) ## age column is having the null values

# cabin and embarked are having missing values as "" 
table(titanic_all$Cabin)
levels(titanic_all$Cabin)[1] = "missing"

# checking for survial distributions
table(titanic_all$Survived)

# Checking the distribution of age column
hist(titanic_all$Age) #normally distributed 

# checking for fare distribution
hist(titanic_all$Fare) #not normally distributed

# checking for outliers
boxplot(titanic_all$Age, horizontal = T) #there are outliers

boxplot(titanic_all$Fare, horizontal = T) #have outliers
 
# median imputation will make more sense as we have outliers
summary(titanic_all) #median is 28

# imputing na with 28
titanic_all$Age[is.na(titanic_all$Age)] <- 28
#age.median <- median(titanic_all$Age, na.rm = T )
# titanic_all$Age[is.na(titanic_all$Age)] <- age.median

# imputing fare with 14
titanic_all$Fare[is.na(titanic_all$Fare)] <- 14.454

# embarked
titanic_all[titanic_all$Embarked == "", "Embarked"] <- "S"

# checking once again
table(is.na(titanic_all)) 
#the null value we created for survival

