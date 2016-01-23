## using the Decision Tree model in R

set.seed(1234)                                               # set seed so random generated numbers can be reproduced.


## Explore the datasets Train and Test


train <- read.csv('a-train.csv',header=T,na.strings=c(""))   # read in both datasets
test <-read.csv('a-test.csv',header=T,na.strings=c(""))


train$Pclass <- ordered(train$Pclass,levels=c("3","2","1"))  # convert the Passenger Class to ordered factor
test$Pclass <- ordered(test$Pclass,levels=c("3","2","1"))    # R will treat factors as nominal variables and ordered factors as ordinal variables in statistical procedures and graphical analyses.


names(train)                                                 # checking columns

data <- subset(train,select = c(2,3,5:8))                    # cleaning and formatting both datasets Train and Test

data$Age[is.na(data$Age)] <- median(data$Age,na.rm=T)        # impute Age where NA, use median of Age


data <- data[!is.na(data$Embarked),]                         # remove the two rows with missing values in Embarked, since there are only two
rownames(data) <- NULL



age.median<- median(test$Age, na.rm=TRUE)                    # remove NA for Age in Test      
test$Age[is.na(test$Age)] = age.median
age.median
mean (test$Age)


## Split the data into a subTrain and test datasets for training and testing the model
######################################################################################
## This supports the holdout validation by splitting the training data into two parts, 
## a training set and a validation set, building a model with the training set and then 
## assessing performance with the validation set.

data = train
dim (train)                                                 # 891 rows/obs 12 var/features

indexes = sample(1:nrow(train), size=0.3*nrow(train)) ## sample indexes (randomize)


test.2 = train[indexes,]                                    # split data 70% Train 30% Test                  
dim(test.2)                                                 # 267 rows/obs (30%)
train.2 = data[-indexes,]
dim(train.2)                                                # 624 rows/obs (70%)



                                                            # install and load the "rpart" package to used generate decision tree models and the "rpart.plot" package to print plots of the trees 
#install.packages("rpart")                                  # uncomment to install package(s)
#install.packages("rpart.plot") 

library (rpart)
library(rpart.plot)

# Grow the tree & predict survival based on gender and passenger class

decision.tree1 <- rpart (Survived ~ Sex + Pclass, data=train.2)


prp(decision.tree1,  faclen=0,split.suffix="?",facsep=" or ")       # plot and dont abbreviate, length of factor level names in splits, add the question(?) mark to splits 

## Interpreting the chart of a "simple" decision tree (decisiontree1)
##################################################################################################################################################################
## The plot shows use that within each gender, the model assigns a lower survival probability to passenger 
## with lower passenger classes: men of class 3 and 2 only have a 14% chance of survival while women of classes 2 and 1 have a 95% chance of survival.

## And below adding more features to the decision tree two

decision_tree2 <- rpart(Survived ~ Sex + Pclass + Age + SibSp,
                     cp = 0.001,                               # set complexity parameter
                     data = train.2)  


## The complexity parameter (cp) governs model complexity. A smaller complexity parameter will allow for more complex models.
#############################################################################################################################
## cp adjusts the improvement of the model fit necessary for it to create a new branch
## As well, the maximum depth of the tree and the minimum number of observations at each leaf node to limit model complexity


cols <- ifelse(decision_tree2$frame$yval == 1, "darkred", "green4") # adjusting the colors of the "leaves" green if survived for plotting

prp(decision_tree2, col=cols,nn.box.col=3)                          # plot the decision tree two (decisiontree2)


decision_tree3 <- rpart(Survived ~ Sex + Pclass + Age + SibSp,      # prune the tree to reduce complexity
                                 cp = 0.001,              # set complexity parameter
                                 maxdepth = 5,            # set maximum tree depth
                                 minbucket = 2,           # set min number of obs in leaf nodes
                                 method = "class",        # return classifications instead of probs
                                 data = train.2)          # use the titanic train.2 dataset


cols <- ifelse(decision_tree3$frame$yval == 1, "darkred", "green4")                  # adjusting the colors of the "leaves" to green if survived for plotting

prp(decision_tree3, col=cols,nn.box.col=3,faclen=0,split.suffix="?",facsep=" or ")   # plot the "pruned" decision tree                            
                                                                                     # plot tree and dont abbreviate length of factors names, add ? to decision splits
                                                                                     # sibsp Number of Siblings/Spouses Aboard
                                                                                     # parch Number of Parents/Children Aboard


## Make predictions

train_preds <- predict(decision_tree3, 
                       newdata=test.2, 
                       type="class")                               # return class predictions
                                                                   # using confusionMatrix () function from caret package, calculates a cross-tabulation of observed and predicted classes with associated statistics.
## Install.packages("caret", dependencies = TRUE)                  # uncomment to install
library (caret)
confusionMatrix(train_preds,test.2$Survived) 
# Confusion Matrix Accuracy of 0.8127    
 

## Manual Confusion Matrix 

table(train_preds, test.2$Survived)

## Confusion Matrix
## Accuracy is 0.8127341
## train_preds            0    1
##                     0 150  37
##                     1  13  67
## total true positives + total true negatives)/total
##  217/267=	0.8127341


## Submit to Kaggle for score

Prediction <- predict(decision_tree3, newdata=test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv (submit, file = "kaggle_decisiont3", row.names = FALSE)

##Kaggle Score# 0.75598

