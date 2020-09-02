###########XXXXX---   LOGISTIC REGRESSION II  ---XXXXXX############

# creating bins for age and fare into factors 
# for age
hist(titanic_all$Age)
boxplot(titanic_all$Age, horizontal = T)
age.cut <- cut(titanic_all$Age, breaks = c(0,14,27,40,54,67,81),
               labels = c("A","B","C","D","E","F"))
age.cut
# for fare
table(titanic_all$Fare>0)
hist(titanic_all$Fare)
boxplot(titanic_all$Fare, horizontal = T)
far.cut <- cut(titanic_all$Fare, breaks =c(-1,10,20,30,61,102,514),
               labels= c("1st","2nd","3rd","4th","5th","6th"))
table(far.cut)

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
agex <- createDummyFeatures(age.cut)
farex<- createDummyFeatures(far.cut)

# storing survived in y 
y <- titanic_all$Survived

# creating a new data base of dummies and y
new_data <- cbind(y,agex,farex,sexx,embarkedx,pclassx,
                  sibspx, parchx, titanic_all$Istrain)

table(is.na(new_data))
summary(new_data)

# partition of data
# spliting into training and testing 
titanic_train <- new_data[new_data$`titanic_all$Istrain` == TRUE,]
titanic_test <- new_data[new_data$`titanic_all$Istrain` ==FALSE,]

#converting survival into factor
titanic_train$y <- as.factor(titanic_train$y)
titanic_train <- titanic_train[-38]
# creating valadation and training data
train_data <- titanic_train[1:780,]
val_data <- titanic_train[781:891,]

## logistic regg model
titanic_logistic <- glm(y~., data= train_data, family = "binomial")
#agex,farex,sexx,embarkedx,pclassx,"sibspx, "parchx"
summary(titanic_logistic)

# train prediction
prob.train <- titanic_logistic$fitted.values

# making confusion matrix
train.confusion <- table(prob.train>0.45, train_data$y)

train.acc <- sum(diag(train.confusion))/sum(train.confusion)*100
train.acc
#80.897 = 0.5
#79.48 = 0.4

# for testing data
prob.val <- predict(titanic_logistic, newdata = val_data)

# prediction on test
confusion.test <- table(prob.val>0.45, val_data$y)

acc.test <- sum(diag(confusion.test))/ sum(confusion.test)*100
acc.test
#83.78 = 0.5
#85.58 = 0.4

## changing the cut off values
library("ROCR")
#rocplot <- prediction(prob.train, train_data$y)
rocrpred <- prediction(prob.train, train_data$y)
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

# predicting on new data 
Survived <- predict(titanic_logistic, newdata = titanic_test)

PassengerId <- 892:1309

Survived[Survived>0.4] = 1
Survived[Survived<=0.4] = 0
Survived

# writing the csv format
output <- as.data.frame(PassengerId)
output$Survived <- Survived

write.csv(output, file = "kaggle.submission.log3.csv", row.names = F)
