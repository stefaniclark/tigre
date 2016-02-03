## using a GLM (General Logistics model) in R 


set.seed(1234)                                       ## set seed so random generated numbers can be reproduced. 

## Explore the datasets Train and Test  
train <- read.csv('a-train.csv',header=T,na.strings=c(""))
test <-read.csv('a-test.csv',header=T,na.strings=c(""))

## Check for missing values and look how many unique values there are for each variable 
##########################################################################################
## using the sapply() function which applies the function passed as argument to each column of the dataframe.


sapply(train,function(x) sum(is.na(x)))             ## examine and show where there are NAs (missing data).  Age has 177 , Cabin has 687 and Embarked has 2 
                                        
sapply(train, function(x) length(unique(x)))        ## examine and show the unique values e.g. four unique values for Embarked, 89 unique ages etc. 




## install.packages ("Amelia")                      ## uncomment to install the Amelia package to to use the the plotting function  missmap() to get a visual of the  visual of the missing data
library(Amelia)                                     ## use libary () function to load package(s)
missmap(train, legend = TRUE, col = c("blue","green"), main = "Missing values(in Blue) vs. Observed (in Green)", y.cex = 0.6, x.cex = 0.6 )

## Intrepreting the chart:
###################################################################
## The variable cabin has too many missing values, so excluding it.
## In addition excluding passengerId  (as it shouldnt have any relavance to survival)
## Using the subset() function to subset the original dataset selecting the relevant columns only.


data <- subset(train,select=c(2,3,5:8,10,12))         ## cleaning and formatting both datasets Train and Test

data$Age[is.na(data$Age)] <- median(data$Age,na.rm=T) ## impute Age where NA, use median of Age


data <- data[!is.na(data$Embarked),]                  ## remove the two rows with missing values in Embarked, since there are only two
rownames(data) <- NULL



age.median<- median(test$Age, na.rm=TRUE)             ## remove NA for Age in Test      
test$Age[is.na(test$Age)] = age.median
age.median
mean (test$Age)


## Split orginal Train set into two subsets train.2 and test.2
##################################################################################################
## The train.2 set will be used to fit a model which we will apply to the test.2 for prediction.
## Holdout validation involves splitting the original training data into two parts, a sub-training set and a test set. 
## then building a model and training it on the sub-train set and then assessing performance on the test set. 

data = train
dim(train)                                                     ## 891 rows/obs 12 var/features

                                                               
indexes = sample(1:nrow(train), size=0.3*nrow(train))          ## sample Indexes (randomize)

                                                               ## split data 70% Train 30% Test
test.2 = train[indexes,]
dim(test.2)                                                    ## 267 rows (30%)
train.2 = data[-indexes,]
dim(train.2)                                                   ## 624 rows (70%)

names(train.2)                                                 ## checking Columns Variables

## Building model                                                            
glmmodel <- glm (Survived ~ Pclass + Sex + Age + SibSp + Parch , data = train.2, family = binomial ('logit'), maxit = 100)

summary (glmmodel)                                             ## review results


## Summary explained: 
##############################################################################################
## we can see that SibSp, Fare and Embarked are not statistically significant.
## As for the statistically significant variables, sex has the lowest p-value suggesting a strong association of the sex of the passenger with the probability of having survived. 
## The negative coefficient for this predictor suggests that all other variables being equal, the male passenger is less likely to have survived. 
## In the logit model the response variable is log odds: ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + . + z*xn. 
## Since male is a dummy variable, being male reduces the log odds by 2.56, and a unit increase in Age reduces the log odds by 0.044


## Make Predictions:
p <- predict(glmmodel, newdata=subset(test.2,select=c(2,3,4,5,6,7,8,10,12)), type="response")
pr <- prediction(p, test.2$Survived)
print (p)

## Confusion Matrix for different thresholds 

table (test.2$Survived,p>0.7)  
## Accuracy using p>0.7		
##     FALSE TRUE
##   0   117    5
##   1    45   42
## 117 + 42/209	= 0.7607656


table (test.2$Survived,p>0.5) # Best score
## Accuracy using p>0.5	
##    FALSE TRUE
## 0     101   21
## 1      23   64
## 101 + 64/209 = 0.7894737

table (test.2$Survived,p>0.3)
##Accuracy using p>0.3						
##      FALSE TRUE
##  0    76   46
##  1    13   74
## 76 + 74 /209 =0.7177033


## Plot
library(ggplot2)
barchart <- ggplot(test.2, aes(as.factor(Pclass), fill=as.factor(Survived)))+geom_bar()

barchart+xlab("Passenger Class")+ylab("Number of Passengers")+ggtitle("Survival by Passenger Class")+scale_fill_discrete(name = "", labels = c("Died", "Survived"))

ggsave("titanic_barchart_submit.png", width = 5, height = 5)




## Submit to Kaggle for score

predict.glm <-predict(glmmodel,  newdata=test, type="response")
test$Survived <- as.numeric(as.numeric(predict.glm)>0.5)
write.csv(test[,c("PassengerId", "Survived")],"Kaggle_glm_submit.csv", row.names=F)


## Kaggle GLM Score 0.74641



## function for assessing binary classification accuracy

glmtable<-table (test.2$Survived,p>0.5)
performance <-function (table, n=2){
  if (!all(dim(table) == c(2,2)))
    stop("table must be 2x2")
  tn = table[1,1]
  fp = table [1,2]
  fn = table [2,1]
  tp = table [2,2]
  sensitivity = tp/(tp +fn)
  specificity = tn/ (tn+fp)
  ppp = tp/ (tp +fp)
  npp = tn/ (tn+fn)
  hitrate = (tp +tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity,n),
                  "\nSpecificity = ", round (specificity, n),        
                  "\nPositive Predictive Value = ", round (ppp, n),  
                  "\nNegative Predictive Value = ", round (npp, n), 
                  "\nAccuracy = ", round (hitrate,n), "\n", sep = "")
  cat(result)        
}



performance(glmtable)  # get performance metrics


#Trying to get more metris
library (ROCR)

y <- ... # logical array of positive / negative cases
predictions <- ... # array of predictions

pred <- prediction(p, test.2$Survived)

# Recall-Precision curve             
RP.perf <- performance(pred, "prec", "rec")

plot (RP.perf)

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr")
plot (ROC.perf,main="ROC curve for GLM with AUC of 0.8481722",col=3,lwd=2)
abline(a=0,b=1,lwd=2,lty=1,col="gray")


# ROC area under the curve
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
#auc [1] 0.8481722

# Variable importance

library(caret)
varImp(glmmodel)
#Overall
#Pclass  7.8713796
#Sexmale 9.9000364
#Age     4.3511155
#SibSp   2.9392165
#Parch   0.3397974


