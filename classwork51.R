## ONE PERSON FROM YOUR GROUP SHOULD SCREEN-SHARE


######## FROM HERE TO THE NEXT SET OF ####### 
######## (BELOW) RUN THE CODE ALL AT ONCE (NOT LINE BY LINE)
install.packages("dplyr")
library(dplyr) 

install.packages("tree")
library(tree)

library(dplyr) # For data manipulation


# Fetch car miles-per-gallon data from the UCI Machine Learning Repository
url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
# https://archive.ics.uci.edu/ml/datasets/auto+mpg

mpg <- read.table(url, stringsAsFactors = FALSE, na.strings="?", header=FALSE)
names(mpg) <- c("mpg","cyl","disp","hp","weight","accel","year","origin","name")

head(mpg)

# Look at the data and reset some of the data types
dim(mpg); summary(mpg)
sapply(mpg,class)
mpg <- mutate(mpg, hp = as.numeric(hp),
              year = as.numeric(year),
              origin = as.factor(origin))
head(mpg,2)

# Check to see if any observations have NAs
sum(is.na(mpg))
which(is.na(mpg == TRUE))

# Omit observations with NAs
mpg <- na.omit(mpg)
#
# Function to divide data into training, and test sets 
index <- function(data=data,pctTrain=0.7)
{
  # fcn to create indices to divide data into random 
  # training, validation/testing data sets
  N <- nrow(data) 
  train <- sample(N, pctTrain*N) 
  test <- setdiff(seq_len(N),train) 
  Ind <- list(train=train,test=test)
  return(Ind)
} 
#
set.seed(123)
ind <- index(mpg, 0.8)
length(ind$train); length(ind$test)

############# OK STOP HERE! FROM HERE ONWARD, RUN IT LINE BY LINE...

### YOUR GOAL IS TO DEVISE A CLASSIFIER THAT PREDICTS WHETHER OR NOT A CAR
### IS MANUFACTURED IN THE USA. HOW DO YOU DO IT?
mpg$origin=ifelse(mpg$origin==1, 1,2)
form <- formula("origin ~ cyl + disp + hp + weight + accel + year + mpg")


### YOU WILL HAVE TO DO A LITTLE "FEATURE ENGINEERING"...
### DON'T JUST RUN THE LINE BELOW AS IT IS... FIX "origin" first...

rf_fit <- tree(formula=form,data=na.omit(mpg[ind$train,]), method = class) # Build the model (use rpart if you wish)

### Use the methodology described in section 8.3 of your textbook to utilize your CART model (above) to:
### (A) Produce a CART plot, with text, and calculate the training set error rate
plot(rf_fit)
text(rf_fit, pretty=0)
trainerror=ifelse(predict(rf_fit)<=1.5, 1,2, type=class())
table(trainerror, na.omit(mpg[ind$train,])$origin)
### (B) Calculate the test set error rate
testerror=ifelse(predict(rf_fit,newdata = mpg[ind$test,])<=1.5, 1,2)
table(testerror, na.omit(mpg[ind$test,])$origin)
### (C) Consider whether pruning the tree might improve accuracy; identify optimal 'alpha'
cv=cv.tree(rf_fit, FUN=prune.misclass)
### (D) Figure out how well this pruned tree performs on the test dataset

### SOURCE:
### 99% of the code above was copy/pasted from 
### http://blog.revolutionanalytics.com/2016/03/confidence-intervals-for-random-forest.html