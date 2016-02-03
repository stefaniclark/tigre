  
getwd()


set.seed(1234)                      ## set seed so random generated numbers can be reproduced.
#install.packages("randomForest")   ## uncomment to install the random forest package
library(randomForest)               ## use the library() function to load a package(s)
#library(caret)


## Explore the datasets Train and Test

train <- read.csv('a-train.csv',header=T,na.strings=c(""))    # read in train dataset

train$Pclass <- ordered(train$Pclass, levels=c("3","2","1"))  # convert to ordered factor
                                 


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
                         mtry=2,                # number of branch variables
                         Importance=TRUE,      # Assess Variable importance
                         keep.forest=TRUE)      # If set to FALSE, the forest will not be retained in the output object. If xtest is given, defaults to FALSE
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


test.2.prediction = predict(rf1, test.2)


table(test.2.prediction, test.2$Survived)

#test.2.prediction       0   1
#                    0 147  34
#                    1  16  70


plot(rf1)          #The mean square error of the random forest
importance(rf1)    # examine the importance of each attribute
varImpPlot(rf1)    #The visualization of variable importance

margins.rf=margin(rf1,train.2)  
plot(margins.rf)   #The margin cumulative distribution graph for the random forest method
hist(margins.rf,main="Margins of Random Forest for titanic dataset")
boxplot(margins.rf~train.2$Survived, main="Margins of Random Forest for titanic dataset by class")



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







##ROC /AUC to work

## formula prediction(predictions, labels, label.ordering = NULL)
rf1 <- randomForest (Survived ~ Pclass + Sex + Age + SibSp + Parch,
                     data= train.2,         # data set
                     ntree=1000,            # number of trees to grow
                     mtry=2,                # number of branch variables
                     Importance=TRUE,
                     keep.forest=TRUE)

## sample reference: adult.rf <-randomForest(income~.,data=data$train, mtry=2, ntree=1000,
##                        keep.forest=TRUE, importance=TRUE,test=data$val)

# train the random forest model

rf1 <- randomForest (Survived ~ Pclass + Sex + Age + SibSp + Parch,
                     data= train.2,         # data set
                     ntree=1000,            # number of trees to grow
                     mtry=2,                # number of branch variables
                     Importance=TRUE,
                     keep.forest=TRUE)



# generate probabilities instead of class labels type="prob" ensures that

rf1.pr = predict(rf1,type="prob",newdata=test.2)[,2]
  
#prediction is ROCR function
library("ROCR")  
rf1.pred = prediction(rf1.pr, test.2$Survived)

#performance in terms of true and false positive rates
rf1.perf = performance(rf1.pred,"tpr","fpr")

#plot the curve
plot(rf1.perf,main="ROC curve for RF with AUC of 0.872198",col=3,lwd=2)
abline(a=0,b=1,lwd=2,lty=1,col="gray")

#compute area under curve (AUC)

auc <- performance(rf1.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
#auc [1] 0.872198

#Variable importance

library(caret)
varImp(rf1)

