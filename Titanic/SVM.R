###################-----    SVM     ------#######################

# converting the necessary into factor
titanic_all$Pclass <- as.factor(titanic_all$Pclass)
titanic_all$Embarked <- as.factor(titanic_all$Embarked)
titanic_all$SibSp <- as.factor(titanic_all$SibSp)
titanic_all$Parch <- as.factor(titanic_all$Parch)

# partition of data
# spliting into training and testing 
titanic_train <- titanic_all[titanic_all$Istrain == TRUE,]
titanic_test <- titanic_all[titanic_all$Istrain ==FALSE,]

# converting survived as factor
titanic_train$Survived <- as.factor(titanic_train$Survived)

# training and valdiation set
train.new <- titanic_train[1:770,]
val.new <- titanic_train[771:891,]

# "rbfdot" , "polydot" , "tanhdot" , "vanilladot" , "laplacedot"
# "besseldot" , "anovadot" , "splinedot" , "stringdot"

##############-----    MODEL    ------###########
library(kernlab)
library(caret)
?ksvm #differt type of kernel
# kernel = rfdot
titanic_model_svm <- ksvm(Survived~ ., 
                          data= titanic_train , 
                          kernel= "rbfdot",
                          C = 1)

# predicting on training data
pred_train_svm <- predict(titanic_model_svm,
                          newdata = train.new)

acc_svm <- mean(pred_train_svm==train.new$Survived)*100
acc_svm
#acc1 = 89.15%

# predicting on valdiation data
pred_val_svm <- predict(titanic_model_svm,newdata = val.new)

acc_svm_val <- mean(pred_val_svm==val.new$Survived)*100
acc_svm_val
# acc1 = 87.29%

# predicting on whole training data
pred.total <- predict(titanic_model_svm, newdata = titanic_train)

acc.total <- mean(pred.total==titanic_train$Survived)*100
acc.total

# checking on test data
pred.test <- predict(titanic_model_svm, newdata = titanic_test)

# writing the file back into the kaggle competitaion
PassengerId <- (titanic_test$PassengerId) 
SVMoutput <- as.data.frame(PassengerId)
SVMoutput$Survived <- pred.test

View(SVMoutput)

# writing the file
write.csv(SVMoutput, file = "kaggle.submission.SVM.csv", row.names = F)
