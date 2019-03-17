library(stringr)
library(ggplot2)
library(gridExtra)
loan<-read.csv("loan.csv",stringsAsFactors =F)
str(loan)
#=================================================================
#-- Data Cleaning
#=================================================================

#-----------------------------------------
#-- Treating Missing Values - Step 1
#-----------------------------------------
#There are blank fields/Missing Values in the data which are either proper blank i.e. ""
# or blank space " "

#There are missing values in the data marked as "n/a" or "NA" which are practically strings

#Let's replace all of these cases by NA
loan[loan =="NA" | loan =="N/A"| loan ==""| loan ==" "| loan =="n/a"]<-NA
# We will treat int_rate column as numeric, therefore removing '%' symbol from it
#Column names having only 1 unique value throughout are basically redundent
single_value_column<-names(which(sapply(sapply(loan,FUN=unique),FUN=length)==1))

length(single_value_column)
#We can remove these 60 columns
#From the data set
loan<-loan[,!(colnames(loan) %in% single_value_column)]

#------------------------------------------------------------------------------------------------------------------
#-- Working of Data formatting issues
#------------------------------------------------------------------------------------------------------------------
#We will consider int_rate & revol_util as numeric values, therefore removing '%' symbol from it
loan$int_rate<-as.numeric(str_replace_all(loan$int_rate,"%",""))
loan$revol_util<-as.numeric(str_replace_all(loan$revol_util,"%",""))

#Remove year and years from emp_length.
loan$emp_length<-str_replace_all(loan$emp_length,"years?","")

#Last 4 columns are mostly having zeros and NAs only
#Lets verify:
Onlyzero_orNA<-names(which(sapply(X=loan,FUN=function(x){any(is.na(x)) & all(x==0,na.rm=T)})==TRUE))

#These 3 columns we can remove as they are having only 0 or NA values which wouldn't help us in getting insights
# "collections_12_mths_ex_med" "chargeoff_within_12_mths"  "tax_liens"
#Removing them
loan<-loan[,!(colnames(loan) %in% Onlyzero_orNA)]

#Dates are in Month Year format, Converting them into Date Format
#Date Columns "issue_d","last_credit_pull_d","earliest_cr_line","last_pymnt_d"
Date_col<-c("issue_d","last_credit_pull_d","earliest_cr_line","last_pymnt_d")
loan[,Date_col]<-as.data.frame(lapply(X = loan[,Date_col],FUN = function(x) as.Date(paste0(x, "-1"),format="%b-%y-%d",origin="1970-01-01") ))
#------------------------------------------------------------------------------------------------------------------
#-- Eliminating Columns With More Than 15% Missing Values
#------------------------------------------------------------------------------------------------------------------
#Check for % of NA values in each of the remaining columns
#We will remove columns having more than 15% NAs (removing column not records)
morethan_15pct<-names(which((colSums(sapply(loan,FUN=is.na)==TRUE)*100/sapply(loan, FUN=length))>15))
#"desc" "mths_since_last_delinq" "mths_since_last_record"
loan<-loan[,!(colnames(loan) %in% morethan_15pct)]

#------------------------------------------------------------------------------------------------------------------
#--- Checking for outliers
#------------------------------------------------------------------------------------------------------------------

boxplot1<-ggplot(loan, aes( x=int_rate,y = int_rate)) + geom_boxplot()+
  ggtitle("Interest Rate")+
  labs(y="Interest Rate",subtitle="Checking for Outliers in Interest rate") 
boxplot1

boxplot2<-ggplot(loan, aes( x=annual_inc,y = annual_inc)) + geom_boxplot()+
  ggtitle("Annual Income")+
  labs(y="Annual Income",subtitle="Checking for Outliers in Annual Income") 
boxplot2

grid.arrange(boxplot1,boxplot2,nrow=1,ncol=2)

#We see some extreme values in the interest rate field but they are not necessarily outliers because 
#interest rates can be as high as 25%. We see very few people i.e. 2-3 people are having annual income more than
#2,000,000, 1 person's annual income is 60L but they are just extreme values and not wrong or erroneous
#we shouldn't remove this from the data as these data points just reflect the reality

#Remaining columns 44
#We are interested in some variables among them for our analysis

#"member_id",loan_amnt","funded_amnt","term","int_rate","installment","grade"
#"emp_length","home_ownership","annual_inc","verification_status" ,"issue_d"   
#,"loan_status"
#"purpose","dti","inq_last_6mths","open_acc","revol_util","pub_rec","pub_rec_bankruptcies"
#"addr_state" , "total_pymnt"
Selected_Columns<-c("member_id","loan_amnt","funded_amnt","term","int_rate","installment","grade"
  ,"emp_length","home_ownership","annual_inc","verification_status"    
  ,"issue_d","loan_status",
  "purpose","dti","inq_last_6mths","open_acc","revol_util","pub_rec","pub_rec_bankruptcies","addr_state" , "total_pymnt")

loan<-loan[,(colnames(loan) %in% Selected_Columns)]

#------------------------------------------------------------------------------------------------------------------
#--- Missing Value Imputation
#------------------------------------------------------------------------------------------------------------------

#Now we will check for NA values in the remaining columns
colSums(sapply(loan,FUN=is.na)==TRUE)*100/sapply(loan, FUN=length)
#pub_rec_bankruptcies 1.75%
#revol_util 0.13%
#emp_length 2.71%
#lets impute NA values
loan<-loan[-which(is.na(loan$pub_rec_bankruptcies)==TRUE),]
loan<-loan[-which(is.na(loan$revol_util)==TRUE),]
loan<-loan[-which(is.na(loan$emp_length)==TRUE),]
length(unique(loan$member_id))
#There are 37898 records for 37898 members
#and all records are unique, there are no duplicates

#In loan issue date column only month and year is available.
#We can extract month and year from this column for further analysis
#We have data from June-2007 to Dec-2011

loan$issue_month<-format(loan$issue_d,'%m')
loan$issue_year<-format(loan$issue_d,'%Y')

#-----------------
# Univariate
#-----------------

#First Looking at Distribution of Applicants accross the Loan Status
plot1<-as.data.frame(prop.table(table(Loan_Status=loan$loan_status))*100)%>%
  ggplot(aes(x=Loan_Status,y=Freq,label=Freq))+ 
  geom_col(position = 'dodge',fill="orange")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Loan Status vs No. Of Applicants")+ 
  geom_label(aes(label=paste0(round(Freq,1),' %')),position =position_dodge(width=1))+
labs(x="Loan Status",y="Number of Approved Loans") 
plot1

# There are 3 unique entries in loan_status "Fully Paid","Charged Off"and "Current"
# loan_status with value "Current" will not be used as we don't know whether those will
# be fully paid or become charged off in future (2.9% of total records)

loan<-loan[loan$loan_status!="Current",]

## plotting by excluding Current loan status as it doesnt help much in the current credit risk analysis

plot2<-as.data.frame(prop.table(table(Loan_Status=loan$loan_status))*100)%>%
  ggplot(aes(x=Loan_Status,y=Freq,label=Freq))+ 
  geom_col(position = 'dodge',fill="gold")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Fully Paid & Charged Offs borrowers")+
  geom_label(aes(label=paste0(round(Freq,1),' %')),position =position_dodge(width=1))+
  labs(x="Loan Status",y="Number of Approved Loans",subtitle="Excluding Current Loan Status and 14.3% defaulted the loans")


#14.3% of the total members under consideration are defaulter

grid.arrange(plot1,plot2,nrow=1,ncol=2)


#Let's look at only the charged off population and see which purpose sees the maximum charge offs

as.data.frame(prop.table(table(Purpose=loan[loan$loan_status=='Charged Off',]$purpose))*100)%>%
  ggplot(aes(x=Purpose,y=Freq,label=Freq))+ 
  geom_col(position = 'dodge',fill="orange")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Frequency of Charge Offs or various Loan Purposes")+
  geom_label(aes(label=paste0(round(Freq,1),' %')),position =position_dodge(width=1))+
  labs(x="Purpose",y="Number of Approved Loans")

## We can consider top 5 purpose that influcenes charge off's are
## debt_consolidation, credit_card, other, small_business, other and home improvement

#Next looking at the tendency of the applicants from various categories to charge off
as.data.frame(prop.table(table(Purpose=loan$purpose,loan_status=loan$loan_status),margin=1)*100)%>%
  ggplot(aes(x=Purpose,y=Freq,fill=loan_status,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Frequency of Charge Offs or various Loan Purposes")+
  geom_text(position=position_stack(vjust=0.5),size=4)+ 
  labs(x="Purpose",y="Number of Approved Loans",subtitle="Small business borrowers show 26.8% more likelihood to charge off than other borrowers")

## Results show Small business borrowers show 26.8% more likelihood to charge off than other borrowers

#Let's look at the int rate column:
ggplot(loan, aes(group = loan_status, x = loan_status, y=int_rate)) + geom_boxplot()+
ggtitle("Loan Status vs Interest Rate")+
  labs(x="Loan Status",y="Interest Rate",subtitle="Borrowers who are paying high interest rate are more likely to Charge off") 

#The borrowers who are paying high interest rate are more likely to Charge off


summary(loan$int_rate)

#Min interest charged: 5.42% & Max interest charged: 24.40%
#It's a continuous numeric variable, For ease of analysis we can bucket them into
#several discrete bins

loan$int_rate_bucket<-factor(ifelse(loan$int_rate>=5 & loan$int_rate<=10,"5-10",
                      ifelse(loan$int_rate>10 & loan$int_rate<=15,"10-15",
                      ifelse(loan$int_rate>15 & loan$int_rate<=20,"15-20",
                      "20-25"))),levels = c("5-10","10-15","15-20","20-25"))

as.data.frame(prop.table(table(Interest_Rate=loan$int_rate_bucket,Loan_Status=loan$loan_status),margin=1)*100)%>%
  ggplot(aes(x=Interest_Rate,y=Freq,fill=Loan_Status,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),width=0.65,color="black")+ 
  geom_text(position=position_stack(vjust=0.5),size=5)+
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Charged Off & Fully Paid borrowers For Each Interest Rate Buckets")+
  labs(x="Interest Rates",y="Number of Approved Loans",fill="Loan Status",subtitle="Increase in interest rates also increases defaulters") 

# The graph results shows the rise in interest rate also influences increase in default rate

# According Lenders club data Loan grades based on interest rates. By looking at average ineterest rate for each grade
# It clearly depicts that the hierarchy of loan grades are based on interest rates
# Higer the interest rates, higher the grades will be

tapply(X = loan$int_rate,INDEX = factor(loan$grade),FUN=median) #Since there are extreme values, we are taking median

as.data.frame(prop.table(table(Loan_Grades=loan$grade,Loan_Status=loan$loan_status),margin=1)*100)%>%
  ggplot(aes(x=Loan_Grades,y=Freq,fill=Loan_Status,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black",width = 0.80)+ 
  geom_text(position=position_stack(vjust=0.5))+
  ggtitle("Loan Status based on Grades")+ 
  labs(x="Grades",y="Number of Approved Loans",fill="Loan Status",subtitle="Hirearchy of loan grades based on interest rates")
#Results shows grade of the loans go higher, the interest rate increases and accordingly the likelihood of default increases


## Let's look at the average interest rate for each of the terms
tapply(X = loan$int_rate,INDEX = factor(loan$term),FUN=median) #Since there are extreme values, we are taking median

## It clearly depicts interest rate is higher when borrower opts for maximum number of monthly installments
## Let's look at the frequency of people opting for these 2 kinds

table(loan$term)

## Borrowers mostly opt for 36 months installment

as.data.frame(prop.table(table(Number_Of_Payments=loan$term,Loan_Status=loan$loan_status),margin=1)*100)%>%
  ggplot(aes(x=Number_Of_Payments,y=Freq,fill=Loan_Status,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black",width=0.5)+ 
  geom_text(position=position_stack(vjust=0.5),size=5)+ 
  ggtitle("Loan status based on Term duration")+
  labs(x="Number of Installments",y="Number of Approved Loans",fill="Loan Status",subtitle="Borrowers with \"60 months\" installment has more Defaulters")

# Members who agreed to pay within 60 months are more likely to default(one reason being the higher interest rate for this category)
# than members who pay in 36 monthly installments,


prop.table(table(loan$term))*100

#75% of the total members under consideration aggreed upon 36 months term duration for loan payment
#25% of the total members under consideration aggreed upon 60 months term duration for loan payment

#By Employment Length
prop.table(table(loan$emp_length,loan$loan_status),margin=1)*100
#Across all the employment length, the proportion of fully paid and charged off are almost consistent. Therefore Emp length doesn't have ant strong effect on default rate

#By Home Ownerships
as.data.frame(prop.table(table(HOME=loan[loan$loan_status=='Charged Off',]$home_ownership))*100)%>%
  ggplot(aes(x=HOME,y=Freq,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black")+ 
  geom_text(position=position_stack(vjust=0.5))+ 
  ggtitle("Number of defaulters based on Home Ownership")+ 
  labs(x="Home Ownership",y="Number of Approved Loans") 

#People Paying Rent for their home or who have mortagages are less likely to repay the loan within expected date

#dti is a ratio calculated using the borrower's total monthly debt payments on the total debt obligations, 
#excluding mortgage and the requested LC loan, divided by the borrower's self-reported monthly income.
#Let's see if has any effect
summary(loan$dti)
loan$dti_bucket<-factor(ifelse(loan$dti>=5 & loan$dti<=10,"5-10",
                                    ifelse(loan$dti>10 & loan$dti<=15,"10-15",
                                           ifelse(loan$dti>15 & loan$dti<=20,"15-20",
                                                  ifelse(loan$dti>20 & loan$dti<=25,"20-25","25+"
                                                  )))),levels = c("5-10","10-15","15-20","20-25","25+"))

as.data.frame(prop.table(table(DTI=loan$dti_bucket,loan_status=loan$loan_status),margin=1)*100)%>%
  ggplot(aes(x=DTI,y=Freq,fill=loan_status,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black")+ 
  geom_text(position=position_stack(vjust=0.5))+
  ggtitle("DTI vs Loan Status")+ 
  labs(x="DTI",y="Number of Approved Loans",fill="Loan Status")

#We didn't find anything impactful

loan$inc_bucket<-factor(ifelse(loan$annual_inc>=0 & loan$annual_inc<=25000,"<=25K",
                               ifelse(loan$annual_inc>25000 & loan$annual_inc<=100000,"25K-1L",
                                      ifelse(loan$annual_inc>100000 & loan$annual_inc<=200000,"1L-2L",
                                             ifelse(loan$annual_inc>200000 & loan$annual_inc<=500000,"2L-5L",
                                                    ifelse(loan$annual_inc>500000 & loan$annual_inc<=1000000,"5L-10L","10L+"
                                             ))))),levels = c("<=25K","25K-1L","1L-2L","2L-5L","5L-10L","10L+"))
as.data.frame(prop.table(table(annual_inc=loan$inc_bucket,Loan_Status=loan$loan_status),margin=1)*100)%>%
  ggplot(aes(x=annual_inc,y=Freq,fill=Loan_Status,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black")+ 
  geom_text(position=position_stack(vjust=0.5))+ 
  ggtitle("Defaulters based on Income range")+
  labs(x="Annual Income",y="Number of Approved Loans",fill="Loan Status") 
#people earning less that 25k, 25K-1L & 5-10L are more likely to charge off that others
table(loan$inc_bucket)
# <=25K 25K-1L  1L-2L  2L-5L 5L-10L   10L+ 
#  2182  29286   4785    486     48     13 
#Frequency table shows that most of the applicants earn 0-1L annually
as.data.frame(prop.table(table(annual_inc=loan$inc_bucket,verification_stat=loan$verification_status),margin=1)*100)%>%
  ggplot(aes(x=annual_inc,y=Freq,fill=verification_stat,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black")+ #Creating Stacked bar chart
  geom_text(position=position_stack(vjust=0.5))+ 
  ggtitle("Number of defaulters based on borrowers employment experience")+ 
  labs(x="Emp Length",y="Number of Approved Loans",fill="Loan Status") 

# Although the chart shows that for exceptionally high income also the percentage of unverified income sources are highest
#but we have only 13 persons in that bucket. We are more concerned about the buckets containing more applicants
#i.e. 25k, 25K-1L. Since the income bracket seems very low the income source verification process looks less stringent
#and these applicants tend to charge of later.

#Looking at Inqueries made in last 6 months
loan$inq_bucket<-factor(ifelse(loan$inq_last_6mths>=0 & loan$inq_last_6mths<=1,"Low(No or 1)",
                               ifelse(loan$inq_last_6mths>=2 & loan$inq_last_6mths<=4,"Medium(2-4)","High(>5)"
                               )),levels = c("Low(No or 1)","Medium(2-4)","High(>5)"))
as.data.frame(prop.table(table(inq=loan$inq_bucket,Loan_Status=loan$loan_status),margin=1)*100)%>%
  ggplot(aes(x=inq,y=Freq,fill=Loan_Status,label=paste0(round(Freq,1),' %')))+ 
  geom_col(position = position_stack(),color="black")+ 
  geom_text(position=position_stack(vjust=0.5))+ 
  ggtitle("Loan status vs Each Inquiry range")+ 
  labs(x="Number of Inqueries",y="Number of Approved Loans",fill="Loan Status") 

#As the number of inquries by a loan seeker goes higher, it indicates that he is deperately looking for a credit
#& he might not be capable of repaying it later
ggplot(loan, aes(loan$loan_status, fill = factor(loan$verification_status))) + geom_bar(position = "fill") + 
  labs(title = "Loan status vs verification",
       x = "loan status",
       y = "verification status",fill="Loan Status")

#interesting to see that default cases have more verified applications than in fully paid cases. 
#will need to review the process of verification checks

#Finally, according to our analysis we can conclude that there are 5 drivers that are impacting the
#default rate significantly:
#They are
#1. Loan Interest Rates
#2. Loan Grades
#3. Loan Purposes
#4. Term Duration to repay
#5. Inqueries made by applicants in Last 6 months
#6. Verification Status




