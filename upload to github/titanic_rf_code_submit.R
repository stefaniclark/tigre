## using a RF (Random Forest) model in R 

set.seed(1234)                      ## set seed so random generated numbers can be reproduced.
#install.packages("randomForest")   ## uncomment to install the random forest package
library(randomForest)               ## use the library() function to load a package(s)
library(caret)


## Explore the datasets Train and Test

train <- read.csv('a-train.csv',header=T,na.strings=c(""))   # read in train dataset

train$Pclass <- ordered(train$Pclass,                        # convert to ordered factor
                                levels=c("3","2","1"))  


names(train)                                                  # checking columns

train <- subset(train,select = c(2,3,5:8))                    # cleaning and formatting both datasets Train and Test

train$Age[is.na(train$Age)] <- median(train$Age,na.rm=T)      # impute Age where NA, use median of Age


train$Survived <- as.factor(train$Survived)                  # convert Survived variable to factor



## Split the data into a train.2 and test.2 datasets for training and testing the model
######################################################################################
## This supports the holdout validation by splitting the training data into two parts, 
## a training set and a validation set, building a model with the training set and then 
## assessing performance with the validation set.

data = train
dim (train)                                                    # 891 rows/obs 12 var/features

indexes = sample(1:nrow(train), size=0.3*nrow(train))         # sample indexes (randomize)


test.2 = train[indexes,]                                      # split data 70% Train 30% Test                  
dim(test.2)                                                   # 267 rows/obs (30%)
train.2 = data[-indexes,]
dim(train.2)                                                  # 624 rows/obs (70%)


## Build model on train.2

rf1 <- randomForest (Survived ~ Pclass + Sex + Age + SibSp + Parch,
                         data= train.2,         # data set
                         ntree=1000,            # number of trees to grow
                         mtry=2)                # number of branch variables

rf1                                             # view model summary

## Output from rf1 model summary () function ran on train.2 
## #################################################################################################################################
## Call:
## randomForest(formula = Survived ~ Pclass + Sex + Age + SibSp +      Parch, data = train.2, ntree = 1000, mtry = 2) 
## Type of random forest: classification
## Number of trees: 1000
## No. of variables tried at each split: 2

## OOB estimate of  error rate: 16.83%
## Confusion matrix:
## Accuracy is 0.8317308
##    0    1  class.error
## 0 355  31  0.08031088
## 1  74 164  0.31092437


## Run rf1 model against test.2

rf2 <- randomForest(Survived ~ Sex + Pclass + Age + SibSp + Parch,
                    data= test.2,          # data set
                    ntree=1000,            # number of trees to grow
                    mtry=2)                # number of branch variables

rf2                                        # view model summary

varImpPlot(rf2)                            # plot variable importance
                                           # top 3 vars for Survival are Sex, Age, Pclass

rf2$importance                             # show important variables

## Output from rf2 model summary () function ran on test.2
## #################################################################################################################################
## Call:
## randomForest(formula = Survived ~ Sex + Pclass + Age + SibSp + Parch, data = test.2, ntree = 1000, mtry = 2) 
## Type of random forest: classification
## Number of trees: 1000
## No. of variables tried at each split: 2

## OOB estimate of  error rate: 21.35%
## Confusion matrix:
## Accuracy is 0.7865169
##     0  1   class.error
## 0 140 23   0.1411043
## 1  34 70   0.3269231


# Load and prepare the orginal test dataset to run model on "unknown" data

test <- read.csv('a-test.csv',header=T,na.strings=c(""))          # read in original test dataset

names(test)

test <- subset(test,select = c(1,2,4,5:7))                        # cleaning and formatting both datasets Train and Test

test$Pclass <- ordered(test$Pclass,                               # convert to ordered factor
                               levels=c("3","2","1"))  


test$Age[is.na(test$Age)] <- median(test$Age,na.rm=T)            # impute Age where NA, use median of Age



test_pred <- predict(rf1,newdata = test, type = "class")          
                             
                    

prediction_rf1 <- data.frame(PassengerId=test$PassengerId, Survived = test_pred)


write.csv(prediction_rf1, "kaggle_rf_submit.csv", row.names=FALSE)

##Kaggle Score 0.75120





