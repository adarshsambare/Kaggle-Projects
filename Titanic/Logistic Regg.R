################# LOGISTIC REGRESSION ###########################

# converting the necessary into factor

titanic_all$Pclass <- as.factor(titanic_all$Pclass)
titanic_all$Embarked <- as.factor(titanic_all$Embarked)
titanic_all$SibSp <- as.factor(titanic_all$SibSp)
titanic_all$Parch <- as.factor(titanic_all$Parch)

# changing the class names
levels(titanic_all$Pclass)[c(1,2,3)] <- c("1st Class", "2nd Class","3rd Class")
levels(titanic_all$SibSp) <- c("zerosub","onesub","twosub","threesub","foursub","fivesub","eightsub")
levels(titanic_all$Parch) <- c("zeropar","onepar","twopar","threepar","fourpar","fivepar","sixpar","ninepar")
# creating dummies
library(mlr)
sexx <- createDummyFeatures(titanic_all$Sex)
embarkedx <- createDummyFeatures(titanic_all$Embarked)
pclassx <- createDummyFeatures(titanic_all$Pclass)
sibspx <- createDummyFeatures(titanic_all$SibSp)
parchx <- createDummyFeatures(titanic_all$Parch)

# normalizing the age column
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
} 

norm_age <- norm(titanic_all$Age)
y <- titanic_all$Survived

# creating a new data base of dummies and y
new_data <- cbind(y,norm_age,sexx,embarkedx,pclassx,
                  sibspx, parchx, titanic_all$Istrain)

table(is.na(new_data))
summary(new_data)

# partition of data
# spliting into training and testing 
titanic_train <- new_data[new_data$`titanic_all$Istrain` == TRUE,]
titanic_test <- new_data[new_data$`titanic_all$Istrain` ==FALSE,]

#converting survival into factor
titanic_train$y <- as.factor(titanic_train$y)

# creating valadation and training data
train_data <- titanic_train[1:780,]
val_data <- titanic_train[781:891,]

## logistic regg model
titanic_logistic <- glm(y~., data= train_data, family = "binomial")

summary(titanic_logistic)
# prediction
prob <- titanic_logistic$fitted.values

# making confusion matrix
confusion <- table(prob>0.35, train_data$y)

Accuracy <- sum(diag(confusion))/sum(confusion)*100
Accuracy

# for testing data
prob.test <- predict(titanic_logistic, newdata = val_data)

# prediction on test
confusion.test <- table(prob.test>0.35, val_data$y)

acc.test <- sum(diag(confusion.test))/ sum(confusion.test)*100
acc.test

## changing the cut off values
library("ROCR")
rocplot <- prediction(prob.test, trainx$y)
rocrpred <- prediction(prob.test, trainx$y)
rocrperf <- performance(rocrpred,'tpr','fpr')

##ploting rocr curve##
windows()
plot(rocrperf,colorize = T, text.adj=c(-0.2,1.7), print.cutoffs.at = seq(0.1, by=0.1) )

## and get the required cutoff value

#Data Frame for cutoff values and tpr-fpr
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr = rocrperf@x.values,tpr= rocrperf@y.values)

#column names for the dataframe
colnames(rocr_cutoff)<- c("cutoff","FPR","TPR")

#Rounding values to two decimals
rocr_cutoff <- round(rocr_cutoff,2)

#arrange tpr#
library(dplyr)
rocr_cutoff<- arrange(rocr_cutoff, desc(TPR))
View(rocr_cutoff)

# prediciting on whole data
predict.all <- predict(titanic_logistic, newdata = titanic_train)

# making confusion matrix
confusion <- table(predict.all>0.35, titanic_train$y)

Accuracy <- sum(diag(confusion))/sum(confusion)*100
Accuracy
getwd()
# predicting on new data 
Survived <- predict(titanic_logistic, newdata = titanic_test)
PassengerId <- 892:1309

Survived[Survived>0.35] = 1
Survived[Survived<=0.35] = 0
Survived

# writing the csv format
output <- as.data.frame(PassengerId)
output$Survived <- Survived

write.csv(output, file = "kaggle.submission.csv", row.names = F)
