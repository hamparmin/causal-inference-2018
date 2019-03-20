#part1
setwd("~/Minerva Year 2/CS112/R-datasets/Titanic")

train <- read.csv("~/Minerva Year 2/CS112/R-datasets/Titanic/all/train.csv")
test <- read.csv("~/Minerva Year 2/CS112/R-datasets/Titanic/all/test.csv")
str(train)
a=table(train$Survived)
prop.table(a)

test$Survived <- rep(0,418)
submit <- data.frame(PassengerId=test$PassengerId, Survived =test$survived)
write.csv(submit, file="theyallperish.csv", row.names = FALSE)

#part2
summary(train$Sex)
prop.table()
test$Survived[test$Sex=="female"] <- 1
train$Child <- 0
train$Child[train$Age<18] <- 1

aggregate(Survived ~ Child+Sex, data=train, FUN=sum)
aggregate(Survived ~ Child+Sex, data=train, FUN=function(x) {sum(x)/length(x)})

train$Fare2 <- "30+"
train$Fare2[train$Fare<30 & train$Fare >=20] <-"20-30"
train$Fare2[train$Fare<20 & train$Fare >=10] <-"10-20"
train$Fare2[train$Fare<10] <-"<10"

aggregate(Survived ~Fare2 +Pclass + Sex, data = train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex=="female"] <- 1
test$Survived[test$Sex=="female" & test$Pclass ==3 & test$Fare>=20] <- 0

#part 3
library(rpart)
fit <-rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train, method ="class")
plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))

