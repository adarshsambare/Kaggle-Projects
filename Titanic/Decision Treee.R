###################-------- DECISION TREE -------#######################

# converting the necessary into factor
titanic_all$Pclass <- as.factor(titanic_all$Pclass)
titanic_all$Embarked <- as.factor(titanic_all$Embarked)
titanic_all$SibSp <- as.factor(titanic_all$SibSp)
titanic_all$Parch <- as.factor(titanic_all$Parch)

# removing unnecessary col from titanic all
titanic_all <- titanic_all[-c(1,3,8,10)]

# spliting into training and testing 
titanic_train <- titanic_all[titanic_all$Istrain == TRUE,]
titanic_test <- titanic_all[titanic_all$Istrain==FALSE,]

# removing unnecessary col from train and test
titanic_train <- titanic_train[-9]
titanic_test <- titanic_test[-9]

levels(titanic_train$Embarked)[4] <- "missing"
levels(titanic_test$Embarked)[4] <- "missing"
levels(titanic_train$SibSp)
#converting survival into factor
titanic_train$Survived <- as.factor(titanic_train$Survived)
str(titanic_train)
# creating a validation data set
trainx <- titanic_train[1:780,]
validation <- titanic_train[781:891,]

# Decision tree
library(tree)
library(C50)

## BUILDING A MODEL ##
titanic_model_5.0 <- C5.0(trainx[,-8], trainx$Survived)

#data5.0_model <- C5.0(data_train[,-y],data_train$y )

windows()
plot(titanic_model_5.0)

# predicting on train data
pred_train <- predict(titanic_model_5.0,trainx)

# train accuracy
train_acc = mean(pred_train==trainx$Survived) * 100
train_acc
# acc = 86.92%

# validation prediction and accuracy
pred_val <- predict(titanic_model_5.0,validation)

val_acc <- mean(pred_val==validation$Survived)*100
val_acc
# acc = 84.68
