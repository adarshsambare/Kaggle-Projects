##################----    KNN     -----###################

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
norm_fare <- norm(titanic_all$Fare)
y <- titanic_all$Survived

# creating a new data base of dummies and y
new_data <- cbind(y,norm_age,norm_fare,sexx,embarkedx,pclassx,
                  sibspx, parchx, titanic_all$Istrain)

# partition of data
# spliting into training and testing 
titanic_train <- new_data[new_data$`titanic_all$Istrain` == TRUE,]
titanic_test <- new_data[new_data$`titanic_all$Istrain` ==FALSE,]

# y as factor in train data
titanic_train$y <- as.factor(titanic_train$y)

# labeling them
train_norm <- titanic_train[2:28]
train_label <- titanic_train$y

# new train and valdiation data
train.new <- train_norm[1:791,]
val.new <- train_norm[792:891,]
# making our test data
train.label <- train_label[1:791]
val.label <- train_label[792:891]

# KNN MODEL
library("class")
library("caret")

#creating the test and trainning accuracy as null
val_acc <- NULL
train_acc <- NULL
#writing a for loop for different values of k
?knn
for (i in seq(1,50,1))
{
  train.pred <- knn(train= train.new,test=train.new,cl=train.label,k=i)
  train_acc <- c(train_acc,mean(train.pred == train.label))
  val.pred <- knn(train = train.new, test = val.new, cl = train.label, k=i)
  val_acc <- c(val_acc,mean(val.pred==val.label))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
windows()
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(1,50,1),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(1,50,1),val_acc,type="l",main="Test_accuracy",col="red")


acc_neigh_df <- data.frame(list(train_acc=train_acc,val_acc=val_acc,neigh=seq(1,50,1)))
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=val_acc,colour="val_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","val_acc"),values = c("train_acc"="green","val_acc"="red"))
#20

# by 40 nearest nebigour
titanic.knn <- knn(train = train.new , test =val.new , cl = train.label, k= 20)

knn.acc <- mean(titanic.knn==val.label)*100
knn.acc

# predicting on test data
test.knn <- knn(train = train.new , test =titanic_test , cl = train.label, k= 20)

# editing before submitting on kaggle
PassengerId <- 892:1309

output <- as.data.frame(PassengerId)
output$Survived <- test.knn 

View(output)

# writing as csv format
write.csv(output, file = "kaggle.submission.knn.csv", row.names = F)
