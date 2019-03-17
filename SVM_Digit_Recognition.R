### Support Vector Machine Maching Learning Assignment ###
##########################################################
# Business understanding - 
# When a user submits image of digit through scanner, tablet 
# or other digital devices.The goal is to build the model using 
# SVM to identify the digit between 0-9          


# Objective-  Required to develop a model using Support Vector 
# Machine which should correctly classify the handwritten
# digits based on the pixel values given as features.
##########################################################


## Importing libraries required
library(caret)
library(ggplot2)
library(kernlab)
library(caret)
library(caTools)
library(gridExtra)
library(readr)
library(iterators)
library(parallel)
library(doParallel)

#### ---  Data understanding and Data preparation    ----####

## Enabling parallel processing
cl <- makeCluster(detectCores()) 
registerDoParallel(cl)  ## Detecting the number of cores in a machine to leverage on parallelism

## Checked the data in excel, there are no column names. Importing data without column names
train_full_data<-read.csv("mnist_train.csv",header = F) 
test_full_data<-read.csv("mnist_test.csv",header = F)

str(train_full_data)  ## 60000 obervations with 785 variables
str(test_full_data)   ## 10000 obervations with 785 variables

## Picking 25% of train data from overall data as recommended by mentor to avoid long running execution
set.seed(100)
train.indices = sample(1:nrow(train_full_data), 0.25*nrow(train_full_data))


train_data = train_full_data[train.indices, ]
test_data = test_full_data

## Checking structure of both train and test datasets
str(train_data) ## 15000 obervations with 785 variables
str(test_data)  ## 10000 observations with 785 variables

## Exploring the data

summary(train_data)
summary(test_data)

## Checking dimensions of the datasets
dim(train_data)
dim(test_data)

## Checking first few rows
View(train_data)
View(test_data)


## Making our target class to factor
train_data$V1<-factor(train_data$V1)
test_data$V1<-factor(test_data$V1)

## Renaming target variable to meaningful column name
colnames(train_data)[1]<-"digits"
colnames(test_data)[1]<-"digits"

## Summary of target variable converted into factor
summary(train_data$digits)
summary(test_data$digits)

## Checking for NA values in both train and test datasets
na_count_train <-sapply(train_data, function(x) sum(length(which(is.na(x)))))
sum(na_count_train)
na_count_test <-sapply(test_data, function(x) sum(length(which(is.na(x)))))
sum(na_count_test)
#############################################################
# No NA values in train and test datasets
# There are no missing values to impute
#############################################################
## Checking for duplicate values

nrow(unique(train_data))  
nrow(unique(test_data))


#############################################################
# Both train and test datasets have unique rows
#############################################################


#### ---  Building a model Using Linear Kernel ----####

Model_linear <- ksvm(digits~ ., data = train_data, scale = FALSE,kernel = "vanilladot")
predict_linear<- predict(Model_linear, test_data)
conf_mat<-confusionMatrix(predict_linear,test_data$digits)
conf_mat
############### The above results shows ####################
## Accuracy  0.9164
## Sensitivity  0.9841
## Specificity 0.9938
############################################################

#### ---  Building a non linear model using RBF Kernel  ----####

Model_RBF <- ksvm(digits~ ., data = train_data, scale = FALSE, kernel = "rbfdot")
predict_RBF<- predict(Model_RBF, test_data)

#confusion matrix - RBF Kernel
conf_mat_rbf<-confusionMatrix(predict_RBF,test_data$digits)
conf_mat_rbf
############### The above results shows ####################
## Accuracy : 0.9642
## Sensitivity: 0.9903
## Specificity: 0.9975
## 
############################################################

#### ---  Hyperparameter tuning and Cross Validation for linear SVM model  ----####

trainControl <- trainControl(method="cv", number=3,allowParallel = TRUE)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"
set.seed(100)

# making a grid of C values 
grid <- expand.grid(C=seq(1, 3, by=1))

# Performing 3-fold cross validation
fit.svm <- train(digits~., data=train_data, method="svmLinear", metric=metric,tuneGrid=grid, trControl=trainControl)

print(fit.svm)

############### The above results shows ####################
# Best tune at C=1, 
# Accuracy - 0.9076671
############################################################


# Plotting "fit.svm" results
plot(fit.svm)

#### ---  Hyperparameter tuning and Cross Validation for non-linear SVM model  ----####


trainControlrbf <- trainControl(method="cv", number=2,allowParallel = TRUE)

## Measuring the accuracy parameter
metricrbf <- "Accuracy"
set.seed(100)

## Preparing grid of "sigma" and C values to find the best accuracy
grid <- expand.grid(.sigma=c(0.63e-7,1.63e-7,2.63e-7), .C=c(1,2,3) )

# Performing 3-fold cross validation
fit.svmrbf <- train(digits~., data=train_data, method="svmRadial", metric=metricrbf,tuneGrid=grid, trControl=trainControlrbf)

# Printing cross validation result
print(fit.svmrbf)
plot(fit.svmrbf)

############### The above results shows ####################
# The final values used for the model were sigma = 2.63e-07 and C = 3
# Accuracy - 0.9448
############################################################

## Validating the above model using test data

evaluate_rbf<- predict(fit.svmrbf, test_data)
conf_matrix_eval <- confusionMatrix(evaluate_rbf, test_data$digits)
conf_matrix_eval

stopCluster(cl)

############### The  results shows #############################
# Model Accuracy is at 0.9739. Which indicates the model is good. 
# Accuracy -    0.9739
# Sensitivity - 0.9908
# Specificity - 0.9982
# The above model indicates which is good model and be able to predict 
# with the new dataset. 
# In order to achieve the objective of finding digit from a given dataset.
# Non-linear model is best suited to predict the digit.
################################################################



