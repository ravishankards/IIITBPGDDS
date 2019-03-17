
###########################################
#### Assignment - Risk Stratification  ####
###########################################

library("MASS")  # StepAIC function
library("car")   # VIF function
library("caret") # confusionMatrix 
library("ggplot2")
library("caTools")  # for sample split


## Reading data from CSV file
wecare_data<-read.csv("diabetic_data.csv",stringsAsFactors = FALSE,na.strings = "?")

## Understanding demographics of the dataset
str(wecare_data)

ncol(wecare_data) # There are 50 columns in the dataset
nrow(wecare_data) # In total there are 101766 observations

## checking for duplicate values
sum(duplicated(wecare_data))

## Checking for blank values in the columns
sum(wecare_data == "" & wecare_data == " ")

## There are columns which indicates medicines consumed by patient which are irrelavant to the this assignment. 
## As our focus on diabetic patients we will use column "diabetic "diabetesMed".
## Removing column max_glu_serum as mentioned by Rohit during the video session

delete_cols<-c("patient_nbr","admission_type_id", "max_glu_serum", "metformin", "repaglinide", 
               "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide","glipizide",
               "glyburide","tolbutamide", "pioglitazone", "rosiglitazone","acarbose","miglitol", 
               "troglitazone","tolazamide","glyburide.metformin", "glipizide.metformin",
               "glimepiride.pioglitazone","metformin.rosiglitazone", "metformin.pioglitazone","citoglipton","examide")

wecare_data <- wecare_data[!names(wecare_data) %in% delete_cols]

## Removing column "encounter_id" which doesnt have significance 

wecare_data <- wecare_data[,-1]
str(wecare_data)


## There are NA values which exists after replacement of character "?"

sapply(wecare_data, function(x) sum(is.na(x)))

## From the results, columns weight, payer_code, medical_specialty, race, diag1, diag2, diag3 contains NA values
## 
## For Column payer_code = 39.55 % of data is not available
## For Column medical_specialty  = 49.08% of data is not available
## For Column weight = 96.85% of data is not available
## Since high amount of data is not available for the columns Columns payer_code, medical_speciality, weight will be dropped from the dataset for further analysis

wecare_data<-wecare_data[!names(wecare_data) %in% c("payer_code","medical_specialty","weight")]
str(wecare_data)

wecare_data <- wecare_data[!is.na(wecare_data$race),] # 2273 observations dropped
wecare_data <- wecare_data[!is.na(wecare_data$diag_1),] # 21 observations dropped
wecare_data <- wecare_data[!is.na(wecare_data$diag_2),] # 358 observations dropped
wecare_data <- wecare_data[!is.na(wecare_data$diag_3),] # 1423 observations dropped

## Now we got rid off all the NA values in the dataset
sum(is.na(wecare_data))

## According rubrics converting "readmitted" column into binary column based on condition 
## >30" and "<30" is Yes else NO
wecare_data$readmitted<-ifelse(wecare_data$readmitted == "NO", 0,1)

table(wecare_data$readmitted)



## Create the derived metric 'comorbidity', according to the following scheme
## We will be considering 3 columns diag_1, diag_2, and diag_3 to dervie comorbidity column 
## Column comorbidity will be populated based on following condition 
## If the value not equal to 250.xx and not between  390-459, then comorbidity value is 0
## if the value equal to 250.xx and not between  390-459, then comorbidity value is 1
## if the value not equal to 250.xx and between 390-459, then comorbidity value is 2
## if the value equal to 250.xx and between 390-459, then comorbidity value is 3


comorb_func=function(row){
  
  ## picking the ICD codes from the 3 diags columns
  diag1<-as.numeric(substring(row[1],1,3))
  diag2<-as.numeric(substring(row[2],1,3))
  diag3<-as.numeric(substring(row[3],1,3))
  
  ## When substring returns NA value it will be assigned to 0 to handle numeric and this will be anyway irrelevant
  diag1[is.na(diag1)]<-0
  diag2[is.na(diag2)]<-0
  diag3[is.na(diag3)]<-0
  
  
  if(diag1!=250 && diag2!=250 && diag3 != 250){
    if(diag1 %in% 390:459 || diag2 %in% 390:459 || diag3 %in% 390:459){
      comor<-2
    } else {
      comor<-0
    }
  }else if(diag1 == 250 || diag2==250 || diag3 == 250){
    if(diag1 %in% 390:459 || diag2 %in% 390:459 || diag3 %in% 390:459){
      comor<-3
    } else {
      comor<-1
    }
  }
  return(comor)
}

## Deriving comorbidity using diag_1, diag_2,diag_3 columns by calling the function
wecare_data$comorbidity<-apply(wecare_data[,c("diag_1","diag_2","diag_3")],MARGIN=1,FUN = comorb_func)
                                  
## Checking the count of dervied comorbidity
table(wecare_data$comorbidity)

## Since the value has been dervied using diag_1, diag_2, diag_3 , dropping these columns from the dataset. 
## The same had been mentioned in the discussion forum
wecare_data<-wecare_data[!names(wecare_data) %in% c("diag_1","diag_2","diag_3")]


## Lets check outliers for the numeric columns


quantile(wecare_data$admission_type_id, seq(0,1,0.1)) 

quantile(wecare_data$discharge_disposition_id, seq(0,1,0.1)) 

quantile(wecare_data$admission_source_id, seq(0,1,0.1)) 

quantile(wecare_data$time_in_hospital, seq(0,1,0.1)) 

quantile(wecare_data$num_lab_procedures, seq(0,1,0.1))

quantile(wecare_data$num_procedures, seq(0,1,0.1)) 

quantile(wecare_data$num_medications, seq(0,1,0.1)) 

quantile(wecare_data$number_outpatient, seq(0,1,0.1))

quantile(wecare_data$number_emergency, seq(0,1,0.1)) 

quantile(wecare_data$number_inpatient, seq(0,1,0.1)) 

quantile(wecare_data$number_diagnoses, seq(0,1,0.1)) 

## The data is not heavily skewed on both the sides hence outliers are not treated here.


## converting columns to factor
index<-which(sapply(X = wecare_data,FUN=class)=="character")

index ## These columns will be converted into factor along with readmitted and comorbidity as factor

wecare_data[,c(index,18,19)]<-data.frame(sapply(wecare_data[,c(index,18,19)], function(x) factor(x)))

str(wecare_data)

####################################################################
## Plotting charts to explore the factor columns
####################################################################
ggplot(wecare_data, aes(x=race,fill=readmitted))+ geom_bar(position=position_dodge())
## Caucasian seems to be high in readmission
ggplot(wecare_data, aes(x=gender,fill=readmitted))+ geom_bar(position=position_dodge())
## female patients were not readmitted more 
ggplot(wecare_data, aes(x=age,fill=readmitted))+ geom_bar(position=position_dodge())
## Patients who are readmitted high in the range of age 70-80
ggplot(wecare_data, aes(x=insulin,fill=readmitted))+ geom_bar(position=position_dodge())
## more patients are not taking insulin
ggplot(wecare_data, aes(x=change,fill=readmitted))+ geom_bar(position=position_dodge())
## The chart indicates not signgicant change in medication
ggplot(wecare_data, aes(x=diabetesMed,fill=readmitted))+ geom_bar(position=position_dodge())
## Patients who are taking diabetic medicines are high
ggplot(wecare_data, aes(x=comorbidity,fill=readmitted))+ geom_bar(position=position_dodge())  
# We can from the chart that patients fall under category comorbidity 2


## Plots for numeric columns time_in_hospital , num_lab_procedures,num_procedures,num_medications,number_outpatient
## number_emergency,number_inpatient,number_diagnoses
ggplot(wecare_data, aes(time_in_hospital))+ geom_histogram()
ggplot(wecare_data, aes(num_lab_procedures))+ geom_histogram()
ggplot(wecare_data, aes(num_procedures))+ geom_histogram()
ggplot(wecare_data, aes(num_medications))+ geom_histogram()
ggplot(wecare_data, aes(number_outpatient))+ geom_histogram()
ggplot(wecare_data, aes(number_emergency))+ geom_histogram()
ggplot(wecare_data, aes(number_inpatient))+ geom_histogram()
ggplot(wecare_data, aes(number_diagnoses))+ geom_histogram()


#############################################################################################
#Feature standardisation
#############################################################################################
#Normalising continuous variables
#############################################################################################

wecare_data$time_in_hospital <- scale(wecare_data$time_in_hospital) 
wecare_data$num_lab_procedures <- scale(wecare_data$num_lab_procedures) 
wecare_data$num_procedures <- scale(wecare_data$num_procedures)
wecare_data$num_medications <- scale(wecare_data$num_medications) 
wecare_data$number_outpatient <- scale(wecare_data$number_outpatient) 
wecare_data$number_emergency <- scale(wecare_data$number_emergency) 
wecare_data$number_inpatient <- scale(wecare_data$number_inpatient) 
wecare_data$number_diagnoses <- scale(wecare_data$number_diagnoses) 

wecare_data_mdl <- wecare_data

#############################################################################################
##DUMMY VARIABLE CREATION 
# for using it in logistic regression
#Let us first look at all the factor type variables. 
#We should create dummy variables.
#############################################################################################

str(wecare_data_mdl)
# Columns with having 2 levels

## Converting to factor level 1 and 0
wecare_data_mdl$diabetesMed<- ifelse(wecare_data_mdl$diabetesMed=="Yes",1,0)
wecare_data_mdl$change<- ifelse(wecare_data_mdl$change=="Ch",1,0)

## Converting other columns into factor 
index_1 <- which(colnames(wecare_data) %in% 
                   c('race','gender', 'age', 'discharge_disposition_id','admission_source_id', 
                     'A1Cresult', 'insulin', 'comorbidity'))

wecare_data_mdl_chr <- wecare_data[, index_1]

wecare_data_mdl_chr<- data.frame(sapply(wecare_data_mdl_chr, function(x) if (is.factor(x)) x else factor(x) ))
str(wecare_data_mdl_chr)

## creating dummy variables
dummies <- data.frame(sapply(wecare_data_mdl_chr, 
                             function(x) data.frame(model.matrix(~x-1,data = wecare_data_mdl_chr))[,-1]))


## final Dataset for model building 
wecare_final_data <- cbind(wecare_data[,-c(index_1)],dummies)

str(wecare_final_data) 
## The dataset has 98053 observations with  76 columns

###########################################################################
## splitting the data between test and train
## Train data will have 70% and 30% of test data from the dataset

set.seed(100)
indices= sample.split(wecare_final_data$readmitted, SplitRatio = 0.7)
train=wecare_final_data[indices,]
test=wecare_final_data[!(indices),]

###############################################################################
#logistic regression model building

model_1 <- glm(readmitted ~ ., data = train, family = "binomial")
summary(model_1)
#AIC: 87179  Null deviance: 94838  on 68636  degrees of freedom
#Residual deviance: 87027  on 68561  degrees of freedom

## Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

## Removing  multicollinearity through VIF check
sort(vif(model_2))


## Removing discharge_disposition_id.x7 due to low significance
## model3

model_3<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + change + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
               age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
               discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
               discharge_disposition_id.x18 + discharge_disposition_id.x19 + 
               discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
               discharge_disposition_id.x25 + discharge_disposition_id.x28 + 
               discharge_disposition_id.x3 + discharge_disposition_id.x4 + 
               discharge_disposition_id.x5 + discharge_disposition_id.x6 + admission_source_id.x17 + admission_source_id.x2 + 
               admission_source_id.x20 + admission_source_id.x3 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
               insulin.xNo + insulin.xSteady + insulin.xUp + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)
summary(model_3) 
sort(vif(model_3),decreasing = TRUE)

## Removing discharge_disposition_id.x19  for low signiicance value
## Removing  insulin.xNo for low signiicance value and high VIF value

model_4<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + change + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
               age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
               discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
               discharge_disposition_id.x18 +  
               discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
               discharge_disposition_id.x25 + discharge_disposition_id.x28 + 
               discharge_disposition_id.x3 + discharge_disposition_id.x4 + 
               discharge_disposition_id.x5 + discharge_disposition_id.x6 + admission_source_id.x17 + admission_source_id.x2 + 
               admission_source_id.x20 + admission_source_id.x3 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone +
               insulin.xSteady + insulin.xUp + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_4) 

sort(vif(model_4),decreasing = TRUE)


## Removing discharge_disposition_id.x11,insulin.xUp and change for low signifiance value

model_5<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
               age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
               discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
               discharge_disposition_id.x18 +  
               discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
               discharge_disposition_id.x25 + discharge_disposition_id.x28 + 
               discharge_disposition_id.x3 + discharge_disposition_id.x4 + 
               discharge_disposition_id.x5 + discharge_disposition_id.x6 + admission_source_id.x17 + admission_source_id.x2 + 
               admission_source_id.x20 + admission_source_id.x3 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
               insulin.xNo + insulin.xSteady + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)


summary(model_5) 

sort(vif(model_5),decreasing = TRUE)

## Removing discharge_disposition_id.x18, discharge_disposition_id.x4, discharge_disposition_id.x3, admission_source_id.x17 for low significance value

model_6<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
               age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
               discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + discharge_disposition_id.x15 +  
               discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
               discharge_disposition_id.x25 + discharge_disposition_id.x28 + 
               discharge_disposition_id.x5 + discharge_disposition_id.x6 + admission_source_id.x2 + 
               admission_source_id.x20 + admission_source_id.x3 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
               insulin.xNo + insulin.xSteady + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)


summary(model_6) 

sort(vif(model_6),decreasing = TRUE)

## Removing admission_source_id.x3 for low significance value
## Removing age.x.70.80, insulin.xNo,age.x.60.70.,age.x.80.90, age.x.50.60, insulin.xNo for high  VIF value


## building model 7
model_7<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
               discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + discharge_disposition_id.x15 +  
               discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
               discharge_disposition_id.x25 + discharge_disposition_id.x28 + 
               discharge_disposition_id.x5 + discharge_disposition_id.x6 + admission_source_id.x2 + 
               admission_source_id.x20 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
               insulin.xSteady + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)


summary(model_7) 

sort(vif(model_7),decreasing = TRUE)

## Excluding race.xHispanic,discharge_disposition_id.x28,discharge_disposition_id.x25,due to low significance value


## building model 8
model_8<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xOther + gender.xMale + age.x.40.50. + 
               discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + discharge_disposition_id.x15 +  
               discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
               discharge_disposition_id.x5 + discharge_disposition_id.x6 + admission_source_id.x2 + 
               admission_source_id.x20 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
               insulin.xSteady + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)


summary(model_8) 

sort(vif(model_8),decreasing = TRUE)

## building model 9
## Removing admission_source_id.x9,discharge_disposition_id.x23,discharge_disposition_id.x15,gender.xMale,age.x.40.50.
model_9<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xOther + 
               discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + 
               discharge_disposition_id.x22 +  
               discharge_disposition_id.x5 + discharge_disposition_id.x6 + admission_source_id.x2 + 
               admission_source_id.x20 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               + A1Cresult.x.8 + A1Cresult.xNone + 
               insulin.xSteady + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)


summary(model_9) 

sort(vif(model_9),decreasing = TRUE)


## building model 10
## Removing A1Cresult.x.8,admission_source_id.x2
model_10<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + diabetesMed + race.xAsian + race.xCaucasian + 
               race.xOther + 
               discharge_disposition_id.x13 + 
               discharge_disposition_id.x14 + 
               discharge_disposition_id.x22 +  
               discharge_disposition_id.x5 + discharge_disposition_id.x6 +  
               admission_source_id.x20 + admission_source_id.x4 + 
               admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
               + A1Cresult.xNone + 
               insulin.xSteady + comorbidity.x1 + 
               comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)


summary(model_10) 
## AIC: 88917
##  Null deviance: 94838  on 68636  degrees of freedom
## Residual deviance: 88863  on 68610  degrees of freedom

sort(vif(model_10),decreasing = TRUE)

## Now we have all have variables with high significant values with VIF within 2


## Model Evaluation using logistic regression

wecare_model_lg<-model_10

test_pred = predict(wecare_model_lg, type = "response",newdata = test)

summary(test_pred) 


test$prob_lg<-test_pred

# Let's use the probability cutoff of 50%.

test_pred_readmitted <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))


conf_matrix_1<-confusionMatrix(test_pred_readmitted, test_actual_readmitted, positive = "Yes")

conf_matrix_1
## Specificity 0.43
## Sensitivity 0.77
## Accuracy 0.61


# Let's use the probability cutoff of 40%.
test_pred_readmitted <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_actual_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))

conf_matrix_2<-confusionMatrix(test_pred_readmitted, test_actual_readmitted, positive = "Yes")

conf_matrix_2

# Specificity 0.42
# Sensitivity 0.77
## Accuracy 0.58



##########################################################################
#Let's find out the optimal probalility cutoff 
optimal_cutoff<-function(cutoff)
{
  prediction_readmitted <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  test_actual_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))
  test_conf <- confusionMatrix(prediction_readmitted, test_actual_readmitted, positive="Yes")
  acc<-test_conf$overall[1]
  sens<-test_conf$byClass[1]
  spec<-test_conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out)<-c("sensitivity","specificity","accuracy")
  return(out)
}


#############################################################################################
## create cutoff values from 0.01 to 0.8 and initialise a matrix of 100*3
## Hence reducing the range of cutoff values to help balance the specificity and sensitivity.

## Summary of test probability

summary(test_pred)

s = seq(0.01,0.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = optimal_cutoff(s[i])
} 

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.016)]

cutoff <- mean(cutoff)

cutoff
## The cutoff value is  0.44
## Now lets try with cut off value 0.4488889 to rebuild the model

cutoff_pred_readmitted <- factor(ifelse(test_pred >= 0.4488889, "Yes", "No"))
test_actual_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))

conf_matrix_3<-confusionMatrix(cutoff_pred_readmitted, test_actual_readmitted, positive = "Yes")
conf_matrix_3
## The accuracy now stands at 0.61 for the final model
## Specificity -- 0.61
## Sensitivity -- 0.60

## We will use random forest to build the model

library(randomForest)
set.seed(71)


model_rf <- randomForest(readmitted ~ ., data=train, proximity=FALSE,
                         ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)

model_rf


## Model evaluation 


testPred <- predict(model_rf, newdata=test)

table(testPred, test$readmitted)

conf_final_rf <- confusionMatrix(testPred, test$readmitted)

accuracy_rf <- conf_final_rf$overall[1]

accuracy_rf  #63%

## From the results, out of two model evaulation,  random forest predicts 63% accuracy when compared with 61% accuracy by Logistic regression


## Stratifying population into 3 risk buckets based on following conditions
## High risk (Probability of readmission >0.7)
## Medium risk (0.3 < Probability of readmission < 0.7)
## Low risk (Probability of readmission < 0.3)

readmission_prob <- predict(model_rf, newdata=wecare_final_data, type="prob")

## Using the probablity values which was predicted
wecare_final_data$readmission_prob <- readmission_prob[,2]

## Creating buckets based on probability
wecare_final_data$risk_bucket <- ifelse(round(wecare_final_data$readmission_prob,2) > 0.70, "High Risk",
                                 ifelse(round(wecare_final_data$readmission_prob,2) >= 0.30 & 
                                        round(wecare_final_data$readmission_prob,2) <= 0.70, "Medium Risk",
                                  ifelse(round(wecare_final_data$readmission_prob,2) < 0.30, "Low Risk", "")))

## Calculating High risk Probability 

highrisk_prob <- sum(wecare_final_data$risk_bucket == "High Risk")/nrow(wecare_final_data)
highrisk_prob
## 17.6% of population are in high risk of readmission

## Calculating Medium risk probablity 
mediumrisk_prob <- sum(wecare_final_data$risk_bucket == "Medium Risk")/nrow(wecare_final_data)
mediumrisk_prob 

##43.4% of population are in medium risk of readmission

## Calculating low risk probablity
lowRisk_prob <- sum(wecare_final_data$risk_bucket == "Low Risk")/nrow(wecare_final_data)
lowRisk_prob 
## Rounding off to 39.0% of population are in low risk of readmission













