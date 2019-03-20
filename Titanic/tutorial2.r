#part4
setwd("~/Minerva Year 2/CS112/R-datasets/Titanic")

train <- read.csv("~/Minerva Year 2/CS112/R-datasets/Titanic/all/train.csv")
test <- read.csv("~/Minerva Year 2/CS112/R-datasets/Titanic/all/test.csv")

test$Survived <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1],split = "[,.]")[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x)strsplit(x[1],split = "[,.]")[[1]][2])
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

#part 5

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                  data=combi[!is.na(combi$Age),], 
                  method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

library(randomForest)
set.seed(19)

#train$FamilyID2= combi$FamilyID2[train$FamilyID]

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID,
                    data=combi, 
                    importance=TRUE, 
                    ntree=2000)
