#################---  NAIVE BAYES  ---#######################
## converting the necessary into factor

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

## Applying Naive bayes
library("e1071")
?naiveBayes()

titanic_naive <- naiveBayes(Survived~ Pclass+Sex+Age+SibSp+Parch+Fare,
                            data = train.new,
                            na.action = na.pass)
## Default S3 method:)
titanic_naive$levels # factor

# prediction on train data
predict.train <- predict(titanic_naive, train.new)

train_acc_nb <- mean(predict.train == train.new$Survived)*100
train_acc_nb

# prediction on val data
predict.val <- predict(titanic_naive,val.new) 

val.acc.nb <- mean(predict.val== val.new$Survived)*100
val.acc.nb

# giving the total train data
titanic_naive.all <- naiveBayes(Survived~ Pclass+Sex+Age+SibSp+Parch+Fare,
                            data = titanic_train,
                            na.action = na.pass)

# prediction on val data
predict.val.all <- predict(titanic_naive.all, val.new)

acc.all <- mean(predict.val.all==val.new$Survived)*100
acc.all

# prediction on test data
Survived <- predict(titanic_naive.all, titanic_test)
PassengerId <- 892:1309

# writing the csv format
output <- as.data.frame(PassengerId)
output$Survived <- Survived

write.csv(output, file = "kaggle.submission.NB.csv", row.names = F)
