################### RANDOM FOREST ############################


# converting the necessary into factor

titanic_all$Pclass <- as.factor(titanic_all$Pclass)
titanic_all$Embarked <- as.factor(titanic_all$Embarked)
titanic_all$SibSp <- as.factor(titanic_all$SibSp)
titanic_all$Parch <- as.factor(titanic_all$Parch)

# partition of data
# spliting into training and testing 
titanic_train <- titanic_all[titanic_all$Istrain == TRUE,]
titanic_test <- titanic_all[titanic_all$Istrain ==FALSE,]

#converting survival into factor
titanic_train$Survived <- as.factor(titanic_train$Survived)

# creating valadation and training data
train_data <- titanic_train[1:780,]
val_data <- titanic_train[781:891,]

# applying random forest
library(randomForest)
?randomForest
titanic_RF <- randomForest(Survived~ Pclass+Sex+Age+SibSp+Parch+Fare,
                           data = train_data[-1])

titanic_RF$predicted

# training acc
train_predict <- predict(titanic_RF,newdata = train_data)

train.acc <- mean(train_predict==train_data$Survived)*100
train.acc

# valudation acc
val.predict <- predict(titanic_RF,newdata = val_data)

val.acc <- mean(val_data$Survived==val.predict)*100
val.acc

# checking over all accuracy
total.predict <- predict(titanic_RF, newdata = titanic_train)
total.acc <- mean(total.predict==titanic_train$Survived)                     
total.acc

# predicting on test data
test.predict <- predict(titanic_RF, newdata = titanic_test)

# writing the data back as kaggle want the file
# storing survived and passenger if
# saving both
PassengerId <- (titanic_test$PassengerId) 
RFoutput <- as.data.frame(PassengerId)
RFoutput$Survived <- test.predict

View(RFoutput)

# writing the file
write.csv(RFoutput, file = "kaggle.submission.RF.csv", row.names = F)
