library(MASS)
library(car)
library(ggplot2)
library(tidyr)
library(reshape2)

##############################################################################
## Applying EDA on Car pricing Data and preparing data before buiding model
##############################################################################

car_prices<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)

##### Checking the structure of the data  #####

str(car_prices)

### Findings ### : There are 205 observations with 26 columns. Data contains columns are of type integer, number, and Factor


##### Checking for "NA", single valued column,blanks and duplicates  #####

sum(is.na(car_prices))  

single_value_column<-names(which(sapply(sapply(car_prices,FUN=unique),FUN=length)==1))

single_value_column  # There are no single valued columns

sum(car_prices == "" & car_prices == " ") # Checking for blank values in the data set

unique(car_prices)  # All records are unique when compared with original records , there are no duplicates

sum(duplicated(car_prices))

### Findings ### : There are no NA values, blank values, single valued columns and duplicates

##### Checking for "NA", single valued column, and blanks  #####

car_prices<-separate(car_prices,CarName,into=c("carcompany","carmodel"),sep=" ")

summary(car_prices$carcompany)

### Findings ### : Correcting incorrect car company names (maxda,Nissan,porcshce,toyouta,vokswagen,vw) and removing "carmodel" column as its not required

car_prices$carcompany<-gsub("maxda","mazda",car_prices$carcompany)
car_prices$carcompany<-gsub("Nissan","nissan",car_prices$carcompany)
car_prices$carcompany<-gsub("porcshce","porsche",car_prices$carcompany)
car_prices$carcompany<-gsub("toyouta","toyota",car_prices$carcompany)
car_prices$carcompany<-gsub("vokswagen","volkswagen",car_prices$carcompany)
car_prices$carcompany<-gsub("vw","volkswagen",car_prices$carcompany)


summary(car_prices$carcompany)

car_prices<-car_prices[,-4] # Removing carmodel name column as its not required
car_prices<-car_prices[,-1] # Removing column which contains just the ID, which is not relevant for the analysis

## Converting columns into factor
car_prices$carcompany<-as.factor(car_prices$carcompany)
car_prices$symboling<-as.factor(car_prices$symboling)
car_prices$fueltype<-as.factor(car_prices$fueltype)
car_prices$aspiration<-as.factor(car_prices$aspiration)
car_prices$doornumber<-as.factor(car_prices$doornumber)
car_prices$carbody<-as.factor(car_prices$carbody)
car_prices$drivewheel<-as.factor(car_prices$drivewheel)
car_prices$enginelocation<-as.factor(car_prices$enginelocation)
car_prices$enginetype<-as.factor(car_prices$enginetype)
car_prices$cylindernumber<-as.factor(car_prices$cylindernumber)
car_prices$fuelsystem<-as.factor(car_prices$fuelsystem)


##### Identifying outliers for integer and numeric columns  #####


par(mfrow = c(4, 4));

### Plotting box plot to understand the outliers for each mentioned column

cols<-c("wheelbase","carlength","carwidth","carheight","curbweight","enginesize","boreratio","stroke","compressionratio","horsepower","peakrpm","citympg","highwaympg")

for(i in cols){
  boxplot(car_prices[i],xlab = i,col=rgb(0.3,0.5,0.4,0.6))
} 

### Findings ### : Based on results columns "wheelbase", "carlength", "carwidth","enginesize","stroke","compressionratio","horsepower","peakrpm","citympg","highwaympg"
             ###   has outliers, next step is to clear the outliers for mentioned columns

quantile(car_prices$wheelbase,seq(0,1,0.05)) 
car_prices$wheelbase[which(car_prices$wheelbase>110)]<-110

quantile(car_prices$carlength,seq(0,1,0.01))
car_prices$carlength[which(car_prices$carlength<155)]<-155
car_prices$carlength[which(car_prices$carlength>202.480)]<-202.480


quantile(car_prices$carwidth,seq(0,1,0.01))
car_prices$carwidth[which(car_prices$carwidth>71.700)]<-71.700


quantile(car_prices$enginesize,seq(0,1,0.01))
car_prices$enginesize[which(car_prices$enginesize<90)]<-90
car_prices$enginesize[which(car_prices$enginesize>201.20)]<-201.20

quantile(car_prices$stroke,seq(0,1,0.01))
car_prices$stroke[which(car_prices$stroke<2.6400)]<-2.6400
car_prices$stroke[which(car_prices$stroke>3.9000)]<-3.9000  # Removed all significant outliers


quantile(car_prices$compressionratio,seq(0,1,0.01))
car_prices$compressionratio[which(car_prices$compressionratio>10.9400)]<-10.9400


quantile(car_prices$horsepower,seq(0,1,0.01))
car_prices$horsepower[which(car_prices$horsepower>184.00)]<-184.00


quantile(car_prices$peakrpm,seq(0,1,0.01))
car_prices$peakrpm[which(car_prices$peakrpm>6000)]<-6000

quantile(car_prices$citympg,seq(0,1,0.01))
car_prices$citympg[which(car_prices$citympg>38.00)]<-38.00

quantile(car_prices$highwaympg,seq(0,1,0.01))
car_prices$highwaympg[which(car_prices$highwaympg>46.92)]<-46.92


##############################################################################
###      Creating Dummy variables and deriving new metrics
##############################################################################

### Creating Dummy variables based on categorical columns
### "symboling","carcompany","carbody","drivewheel","enginetype","cylindernumber","fuelsystem"

car_prices_1<-car_prices


dummy_1 <- as.data.frame(model.matrix(~symboling - 1, data = car_prices_1))
car_prices_1<-cbind(car_prices_1[-1],dummy_1)


dummy_2 <- as.data.frame(model.matrix(~carcompany - 1, data = car_prices_1))
car_prices_1<-cbind(car_prices_1[-1],dummy_2)


dummy_3 <- as.data.frame(model.matrix(~carbody - 1, data = car_prices_1))
car_prices_1<-cbind(car_prices_1[-4],dummy_3)


dummy_4 <- as.data.frame(model.matrix(~drivewheel - 1, data = car_prices_1))
car_prices_1<-cbind(car_prices_1[-4],dummy_4)


dummy_5 <- as.data.frame(model.matrix(~enginetype - 1, data = car_prices_1))
car_prices_1<-cbind(car_prices_1[-10],dummy_5)


dummy_6 <- as.data.frame(model.matrix(~cylindernumber - 1, data = car_prices_1))
car_prices_1<-cbind(car_prices_1[-10],dummy_6)

dummy_7 <- as.data.frame(model.matrix(~fuelsystem - 1, data = car_prices_1))
car_prices_1<-cbind(car_prices_1[-11],dummy_7)


### converting factors with 2 levels to numerical variables for columns "fueltype","aspiration","doornumber","enginelocation"

levels(car_prices$fueltype)<-c(1,0)
car_prices_1$fueltype<- as.numeric(car_prices$fueltype)

levels(car_prices$aspiration)<-c(1,0)
car_prices_1$aspiration<- as.numeric(car_prices$aspiration)

levels(car_prices$doornumber)<-c(4,2)
car_prices_1$doornumber<- as.numeric(car_prices$doornumber)


levels(car_prices$enginelocation)<-c(1,0)
car_prices_1$enginelocation<- as.numeric(car_prices$enginelocation)


## Checking for any values after adding Dummy variables

sum(is.na(car_prices_1)) ## No values are found after adding dummy variables 

### Creating Derived metrics ###

car_prices_1$avgmpg<-(car_prices_1$citympg+car_prices_1$highwaympg)/2
car_prices_1$strokeratio<-car_prices_1$boreratio/car_prices_1$stroke


### Creating trained and test data set

set.seed(100)

train_indicies<-sample(1:nrow(car_prices_1), 0.7*nrow(car_prices_1))
train_data<-car_prices_1[train_indicies,]
test_data<-car_prices_1[-train_indicies,]

### Building model using train data

model_1<-lm(price~.,data=train_data)

summary(model_1)

### Summary of model_1 R-squared:  0.978,	Adjusted R-squared:  0.9618 
### Running StepAIC 

step<-stepAIC(model_1, direction="both")
step

model_2<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanydodge + carcompanyjaguar + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot + carcompanyplymouth + 
              carcompanysubaru + carcompanytoyota + carbodyconvertible + 
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)

summary(model_2)
##  R-squared:  0.9757,	Adjusted R-squared:  0.9698 
vif(model_2)
### Removing carcompanyplymouth  because of high P values 0.181278

model_3<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanydodge + carcompanyjaguar + carcompanymazda + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot +  
              carcompanysubaru + carcompanytoyota + carbodyconvertible + 
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)


summary(model_3)

### R-squared:  0.9753,	Adjusted R-squared:  0.9695 
### Removing carcompanymazda because of high P-Values 0.195794

vif(model_3)
### curbweight has high value, but it is significant according to summary

model_4<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanydodge + carcompanyjaguar + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot +  
              carcompanysubaru + carcompanytoyota + carbodyconvertible + 
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)

summary(model_4)
### R-squared:  0.975,	Adjusted R-squared:  0.9694

### Removing carcompanydodge because high P value 0.199099
vif(model_4) 

model_5<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanyjaguar + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot +  
              carcompanysubaru + carcompanytoyota + carbodyconvertible + 
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)

summary(model_5)

###  R-squared:  0.9746,	Adjusted R-squared:  0.9692

vif(model_5)

### Removing carbodyconvertible because high P value 0.155311


model_6<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanyjaguar + carcompanymercury + 
              carcompanymitsubishi + carcompanypeugeot +  
              carcompanysubaru + carcompanytoyota +  
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)

summary(model_6)

###   R-squared:  0.9742,	Adjusted R-squared:  0.9689

vif(model_6)
### Removing carcompanymercury 0.098299 because of high p value 0.126586 


model_7<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanyjaguar +  
              carcompanymitsubishi + carcompanypeugeot +  
              carcompanysubaru + carcompanytoyota +  
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)

summary(model_7)
### R-squared:  0.9736,	Adjusted R-squared:  0.9686 
### Removing carcompanytoyota because of high P-value 0.065718
vif(model_7)


model_8<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanyjaguar +  
              carcompanymitsubishi + carcompanypeugeot +  
              carcompanysubaru +  
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)

summary(model_8)

### R-squared:  0.9729,	Adjusted R-squared:  0.9679 
### Removing wheelbase because of high P value 0.064253
vif(model_8)

model_9<-lm(formula = price ~ aspiration + enginelocation +  
              carwidth + curbweight + boreratio + stroke + horsepower + 
              `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
              carcompanyjaguar +  
              carcompanymitsubishi + carcompanypeugeot +  
              carcompanysubaru +  
              drivewheelfwd + cylindernumbereight + cylindernumberfive + 
              cylindernumberfour + carbodywagon + carcompanyaudi + carcompanyhonda, 
            data = train_data)

summary(model_9)
### R-squared:  0.9721,	Adjusted R-squared:  0.9673
vif(model_9)

### Removing carbodywagon because of high P value 0.059010


model_10<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + boreratio + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanymitsubishi + carcompanypeugeot +  
               carcompanysubaru +  
               drivewheelfwd + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + carcompanyaudi + carcompanyhonda, 
             data = train_data)

summary(model_10)

### R-squared:  0.9713,	Adjusted R-squared:  0.9665

vif(model_10)
### Removing carcompanypeugeot because of high P value 0.041892

model_11<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + boreratio + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanymitsubishi +   
               carcompanysubaru +  
               drivewheelfwd + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + carcompanyaudi + carcompanyhonda, 
             data = train_data)

summary(model_11)
###  R-squared:  0.9703,	Adjusted R-squared:  0.9657  

vif(model_11)
### Removing cylindernumberfive because of high P value 0.050162

model_12<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + boreratio + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanymitsubishi +   
               carcompanysubaru +  
               drivewheelfwd + cylindernumbereight + 
               cylindernumberfour + carcompanyaudi + carcompanyhonda, 
             data = train_data)

summary(model_12)
###  R-squared:  0.9693,	Adjusted R-squared:  0.9649
vif(model_12)
### Removing carcompanymitsubishi because of high P value 0.033561


model_13<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + boreratio + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               drivewheelfwd + cylindernumbereight + 
               cylindernumberfour + carcompanyaudi + carcompanyhonda, 
             data = train_data)

summary(model_13)
### R-squared:  0.9682,	Adjusted R-squared:  0.9638
vif(model_13)
### Removing carcompanyaudi because of high P value 0.371433


model_14<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + boreratio + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               drivewheelfwd + cylindernumbereight + 
               cylindernumberfour + carcompanyhonda, 
             data = train_data)

summary(model_14)
### R-squared:  0.9666,	Adjusted R-squared:  0.9623 
vif(model_14)
### Removing boreratio because of high P value 0.034465

model_15<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               drivewheelfwd + cylindernumbereight + 
               cylindernumberfour + carcompanyhonda, 
             data = train_data)

summary(model_15)
### R-squared:  0.9654,	Adjusted R-squared:  0.9613
vif(model_15)
### Removing drivewheelfwd because of high P value 0.061633

model_16<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               cylindernumbereight + 
               cylindernumberfour + carcompanyhonda, 
             data = train_data)

summary(model_16)
###  R-squared:  0.9644,	Adjusted R-squared:  0.9605 
vif(model_16)
### Removing carcompanyhonda because of high P value 0.034510

model_17<-lm(formula = price ~ aspiration + enginelocation +  
               carwidth + curbweight + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               cylindernumbereight + 
               cylindernumberfour, 
             data = train_data)

summary(model_17)
### R-squared:  0.9631,	Adjusted R-squared:  0.9594
vif(model_17)
### Removing aspiration because of high P value 0.012678



model_18<-lm(formula = price ~ enginelocation +  
               carwidth + curbweight + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               cylindernumbereight + 
               cylindernumberfour, 
             data = train_data)

summary(model_18)
### R-squared:  0.9613,	Adjusted R-squared:  0.9577
vif(model_18)
### Removing cylindernumberfour because of high P value 0.018493


model_19<-lm(formula = price ~ enginelocation +  
               carwidth + curbweight + stroke + horsepower + 
               `carcompanyalfa-romero` + carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               cylindernumbereight
               , 
             data = train_data)

summary(model_19)
###  R-squared:  0.9596,	Adjusted R-squared:  0.9562  
vif(model_19)
### Removing carcompanyalfa-romero because of high P value 0.207297


model_20<-lm(formula = price ~ enginelocation +  
               carwidth + curbweight + stroke + horsepower + 
               carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               carcompanysubaru +  
               cylindernumbereight, 
             data = train_data)

summary(model_20)
### R-squared:  0.9573,	Adjusted R-squared:  0.9541 
vif(model_20)
### Removing  carcompanysubaru because of high P value 0.002519

model_21<-lm(formula = price ~ enginelocation +  
               carwidth + curbweight + stroke + horsepower + 
               carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               cylindernumbereight, 
             data = train_data)

summary(model_21)
### R-squared:  0.9543,	Adjusted R-squared:  0.9512
vif(model_21)
### Removing stroke because of high P value 0.038675

model_22<-lm(formula = price ~ enginelocation +  
               carwidth + curbweight + horsepower + 
               carcompanybmw + carcompanybuick + 
               carcompanyjaguar +  
               cylindernumbereight, 
             data = train_data)

summary(model_22)
### R-squared:  0.9528,	Adjusted R-squared:   0.95
vif(model_22)



### Now we have all significant variables and though VIF values for car width and curb weight are high but P value are less 0.05

predict_value<-predict(model_22,test_data[,-19])
test_data$predicted_values<-predict_value
rel<-cor(test_data$price,test_data$predicted_values)
rel
final_value<-rel^2
final_value

### The Adjusted R- squared is 0.9323014 and the predicted model value is 0.8691858. The prediction by model is fairly good. 
### From the prediction model its infers, 
### The driving attributes for Geely Auto are as below
# enginelocation
# carwidth
# curbweight
# horsepower
# carcompanybmw        
# carcompanybuick      
# carcompanyjaguar     
# cylindernumbereight

## For car specification,Company Geely should consider enginelocation, carwidth, curbweight ,horsepower and eight cylinders while designing the car for American market.
## From pricing perspective they should be competetive enough to car companies like BMW, BUICK, and JAGUAR. 










































