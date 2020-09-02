#################### SALES FORCAST EDA ##########################
setwd("C:/Users/Adarsh Sambare/Documents/GitHub/M5-FORCASTING-WALMAT")
getwd()
# Uploading the data
sellprice <- read.csv(choose.files())
calender <- read.csv(choose.files())

View(sellprice)
View(calender)

attach(sellprice)
attach(calender)

# creating a new data base
train <- as.data.frame(sellprice$store_id)

# rename the colmns
names(train) <- "store_id"
train$validation <- "validation"

# Create columns as per required
library(tidyr)
x1 <- separate(sellprice,col = item_id,into = c("deptid","num1","num2"),sep = "_")

x2 <- unite(sellprice,id1,item_id,store_id,sep = "_")
x3 <- unite(,id,id1,validation, sep = "_")
x4 <- t(calender$d)
x5 <- unite(x4,dept_id,deptid,num2, sep = "_")
View(x5)
View(train)

