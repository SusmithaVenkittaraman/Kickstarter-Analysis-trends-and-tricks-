#setting the directory for data
getwd()
setwd(choose.dir())
#reading the data and basic analysis of observations
kickstarter= read.csv("StarterBook.csv")
summary(kickstarter)
#adding a variable success(0 or 1) based on state and deleting the live kickstarter campaigns
library(dplyr)
library(caret)
kickstarter <- mutate(kickstarter, success= ifelse(kickstarter$state=="successful",1,0))
kickstarter = filter(kickstarter, kickstarter$state == "successful" | kickstarter$state == "canceled" | kickstarter$state == "failed")
#coverting categorical variables to factors
kickstarter$Category=as.factor(kickstarter$Category)
kickstarter$Sub.Category=as.factor(kickstarter$Sub.Category)
kickstarter$staff_pick=as.factor(kickstarter$staff_pick)
kickstarter$spotlight=as.factor(kickstarter$spotlight)
#converting dates to proper format
kickstarter$deadline= as.Date(as.POSIXct(kickstarter$deadline, origin="1970-01-01"))
kickstarter$launched_at= as.Date(as.POSIXct(kickstarter$launched_at, origin="1970-01-01"))

#splitting data as testing and training set
set.seed(144)
library(caTools)
split = sample.split(kickstarter$success, SplitRatio = 0.7)
train = subset(kickstarter, split == TRUE)
test = subset(kickstarter, split == FALSE)
prop.table(table(train$success))
prop.table(table(test$success))

#logestic regression
model1=glm(success~goal+staff_pick+spotlight+Category, data = train, family = binomial)
summary(model1)
predict_Test=predict(model1, type="response", newdata = test)
table(test$success,predict_Test>0.55)
accuracy=(528+632)/(528+36+23+632)
baseline=(503+61)/(503+16+61+639)


#CART
library(rpart)
library(rpart.plot)
model2= rpart(success~goal+Category+staff_pick+Sub.Category, data = train, method = "class", minbucket=25)
prp(model2)
PredictCART= predict(model2, newdata = test, type = "class")
table(test$success, PredictCART)
accuracy2=(426+615)/(426+615+138+40)

#Random Forest
library(randomForest)
set.seed(200)
model3=randomForest(success~goal+Category+staff_pick+Sub.Category, data = train, nodesize=25, ntree= 200)
train$success= as.factor(train$success)
test$success= as.factor(test$success)
predictforest= predict(model3, newdata = test)
table(test$success, predictforest)
accuracyForest=(452+606)/(452+606+112+49)

