#
# Set working directory and import datafiles
#
setwd
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
#
# Construction des features: Title, FamilySize, Child, Fare2 and Cabin1stletter
#
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Child <- 0
combi$Child[train$Age < 14] <- 1
combi$Fare2 <- '30+'
combi$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
combi$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
combi$Cabin1stletter <- NA
combi$Cabin1stletter <- substr(combi$Cabin, start=1, stop=1)
# 
# Train and Test tables plit
#
train <- combi[1:891,]
test <- combi[892:1309,]
#
# Decision tree / Train table
#
library(rpart)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Cabin1stletter + Title + FamilySize,data=train,method="class")
fancyRpartPlot(fit)
#
# Prediction / Test table
#
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "my2ndtree.csv", row.names = FALSE)