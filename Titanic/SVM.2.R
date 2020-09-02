##################---      SVM     ---#########################

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

# Adding the age and fare as factor in data
titanic_all$Age.factor <- age.cut
titanic_all$Fare.factor <- far.cut

# dropping the age and fare columne
titanic_all <- titanic_all[-c(3,6)]
# partition of data
# spliting into training and testing 
titanic_train <- titanic_all[titanic_all$Istrain == TRUE,]
titanic_test <- titanic_all[titanic_all$Istrain ==FALSE,]

# converting survived as factor
titanic_train$Survived <- as.factor(titanic_train$Survived)

# training and valdiation set
train.new <- titanic_train[1:770,]
val.new <- titanic_train[771:891,]

################-----        MODEL          ------###########
library(kernlab)
library(caret)
titanic_model_svm <- ksvm(Survived~ ., 
                          data= titanic_train , 
                          kernel= "rbfdot",
                          C = 1)
# predicting on train data
pred_train_svm <- predict(titanic_model_svm,
                          newdata = train.new)
# accuracy
acc_svm <- mean(pred_train_svm==train.new$Survived)*100
acc_svm
# 83.37

# predicting on valdiation data
pred_val_svm <- predict(titanic_model_svm,newdata = val.new)

acc_svm_val <- mean(pred_val_svm==val.new$Survived)*100
acc_svm_val
# acc1 = 85.95%

# predicting on whole training data
pred.total <- predict(titanic_model_svm, newdata = titanic_train)

acc.total <- mean(pred.total==titanic_train$Survived)*100
acc.total
#83.72

# checking on test data
pred.test <- predict(titanic_model_svm, newdata = titanic_test)

# writing the file back into the kaggle competitaion
PassengerId <- 892:1309
SVMoutput <- as.data.frame(PassengerId)
SVMoutput$Survived <- pred.test

View(SVMoutput)

# writing the file
write.csv(SVMoutput, file = "kaggle.submission.SVM.2.csv", row.names = F)
