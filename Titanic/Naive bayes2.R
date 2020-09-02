# creating bins for age and fare into factors 
# for age
hist(titanic_all$Age)
boxplot(titanic_all$Age, horizontal = T)
age.cut <- cut(titanic_all$Age, breaks = c(0,20,27,33,48,81),
               labels = c("A","B","C","D","E"))
table(age.cut) 
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

# adding fare and age factor col
titanic_all$Age.factor <- age.cut
titanic_all$Fare.factor <- far.cut

# summary & structure
summary(titanic_all)
str(titanic_all)

# partition of data
# spliting into training and testing 
titanic_train <- titanic_all[titanic_all$Istrain == TRUE,]
titanic_test <- titanic_all[titanic_all$Istrain ==FALSE,]

# converting survived as factor
titanic_train$Survived <- as.factor(titanic_train$Survived)

# training and valdiation set
train.new <- titanic_train[1:770,]
val.new <- titanic_train[771:891,]

# applying naive bayes
library("e1071")
?naiveBayes()

titanic_naive <- naiveBayes(Survived~ Pclass+Sex+Age.factor+SibSp+Parch+Fare.factor,
                            data = train.new,
                            na.action = na.pass)
# prediction on train data
predict.train <- predict(titanic_naive, train.new)

train_acc_nb <- mean(predict.train == train.new$Survived)*100
train_acc_nb

# prediction on val data
predict.val <- predict(titanic_naive,val.new) 

val.acc.nb <- mean(predict.val== val.new$Survived)*100
val.acc.nb

# prediction on test data
Survived <- predict(titanic_naive.all, titanic_test)
PassengerId <- 892:1309

# writing the csv format
output <- as.data.frame(PassengerId)
output$Survived <- Survived

write.csv(output, file = "kaggle.submission.NB.2.csv", row.names = F)
