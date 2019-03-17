#Checkpoint 1: Data Cleaning 1
library(dplyr)
library(stringr)
library(tidyr)
# Verifying the directory
getwd()

#Reading Data Files
#-------------------------------------------------------------------------------------
companies <- read.delim("companies.txt",header=TRUE,sep="\t",stringsAsFactors = F)
rounds2<-read.csv(file = "rounds2.csv",header = T,stringsAsFactors = F)

# Removing leading & trailing spaces and converting all company_permalink & permalink to lower case
#Since they are in mixed cases and we might face trouble while joining them
#---------------------------------------------------------------------------------------------------
rounds2$company_permalink<-str_trim(tolower(rounds2$company_permalink))
companies$permalink<-str_trim(tolower(companies$permalink))


#How many unique companies are present in rounds2? 
#------------------------------------------------------------------
length(unique(rounds2$company_permalink))   #-- 66368

#How many unique companies are present in the companies file?
#--------------------------------------------------------------------
length(unique(companies$permalink))       #-- 66368

#Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
length(rounds2[!(rounds2$company_permalink %in% companies$permalink),1]) #--0. Therefore N

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame.
#-------------------------------------------------------------------------------------------------------------------
master_frame<-merge(x = rounds2,y = companies,by.x = "company_permalink",by.y = "permalink", all.x = T)

##How many observations are present in master_frame ?
nrow(master_frame)      #---114949

#Checking for NAs in raised amount usd column
length(which(is.na(master_frame$raised_amount_usd)==TRUE))/length(master_frame$raised_amount_usd)

#--0.1739032 i.e. 17% missing values in raised amount column

# Since there are NA's in the raised_amount_usd column, replacing them with zeros.
#-----------------------------------------------------------------------------------
master_frame[which(is.na(master_frame$raised_amount_usd)==TRUE),]$raised_amount_usd<-0

#=======================================================================================
#Checkpoint 2: Funding Type Analysis
#=======================================================================================

#Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity)
#-----------------------------------------------------------------------------------------------------------------------

avg_fund_amt<-arrange(filter(aggregate(x = master_frame$raised_amount_usd,list(master_frame$funding_round_type),mean)
                                    ,Group.1 %in% c("angel","seed","venture","private_equity")),by=desc(x))
colnames(avg_fund_amt)<-c("fund_type","avg_fund")    #Naming the columns for better understanding

avg_fund_amt #--Display Avg Amount for each of the four funding types

suitable_investment_type<-avg_fund_amt[which(avg_fund_amt$avg_fund>= 5000000 & avg_fund_amt$avg_fund<= 15000000),1]
suitable_investment_type   #--"venture"

#======================================================================================================================
#Checkpoints - Part 2
#====================================
#Checkpoint 3: Country Analysis
#=======================================================================================================================

#Checking for Blanks in country code
length(which(master_frame$country_code==""))/length(master_frame$country_code)
#-- 0.07549435 i.e. 7.5% blanks in country code

#Replacing blanks in country code with "Unknown"
#-------------------------------------------------
master_frame[which(master_frame$country_code==""),]$country_code<-"Unknown"

#-- Grouping by Country Code and suitable funding type i.e. "venture" & summarising the investment amount

country_groups<-group_by(subset(master_frame,funding_round_type=="venture" ),country_code)
tot_fund_summary<-data.frame(summarise(country_groups,tot_fund=sum(raised_amount_usd)))
# Extracting top 9 countries based on total funding amount 
top9<-head(arrange(tot_fund_summary, by=desc(tot_fund)),9)
top9

# Top 3 English Speaking Countries: (Excluding "Unknown"/"Blanks" since we don't know if it's a valid english speaking country or not)
#USA - United States
#GBR - Great Britain
#IND - INDIA

#======================================================================================================================
#Checkpoint 4: Sector Analysis 1
#======================================================================================================================

master_frame<-separate(master_frame,category_list,sep="\\|",into=c("primary_sector"))
mapping<-read.csv("mapping.csv",header = T,stringsAsFactors = F)

#---Converting wide data into long data
#----------------------------------------------------------------------------------------------------------------------
mapping<-subset(gather(mapping,key="main_sector",value="is_main",2:10),is_main==1 ,select=c(1,2))

#---For the ease of joining converting primary_sector in master_frame & category_list in mapping to lower case
#----------------------------------------------------------------------------------------------------------------------
master_frame$primary_sector<-tolower(str_trim(master_frame$primary_sector))
mapping$category_list<-tolower(str_trim(mapping$category_list))

#-------Cleansing the data which has 0 instead of 'na' in category_list column of data frame 'mapping'

mapping$zeros <- str_detect(mapping$category_list, "0")
mapping$zeros <- str_count(mapping$category_list, "0")

mapping$category_list <- str_replace(mapping$category_list, "[0]", "na")
mapping$zeros <- str_count(mapping$category_list, "0")


mapping$category_list <- str_replace_all(mapping$category_list,"[0]", "na")
mapping$zeros <- str_count(mapping$category_list, "0")

#During cleansing this entry got affected so replacing it with the actual string

mapping$category_list[which(mapping$category_list=="enterprise 2.na")] <- "enterprise 2.0"

#-- Removing the zeros column we created for cleansing
mapping<-mapping[,-3]




#Checking for Blanks in Category list:
length(which(mapping$category_list==""))/length(mapping$category_list)
#--0.1%
#Since we don't have any business reason or justification to have empty category we are assuming them to be 'Blanks'
#--- Replacing "" in category list with 'Blanks' string
mapping[which(mapping$category_list==""),]$category_list<-'Blanks'

# Joining master_frame & mapping (Left Join)
#------------------------------------------------------------------------------------------------------------------------
master_frame_merged<-merge(master_frame,mapping,by.x="primary_sector",by.y="category_list",all.x =T)



#=========================================================================================================================
#Checkpoint 5: Sector Analysis 2
#=========================================================================================================================
FT<- "venture"
Country_1<-"USA"
Country_2<- "GBR"
Country_3<-"IND"

#-- D1
sector_groups<-group_by(subset(master_frame_merged,funding_round_type==FT & country_code ==Country_1 &  raised_amount_usd>=5000000 & raised_amount_usd<=15000000),main_sector)
D1<- subset(master_frame_merged,country_code==Country_1 & funding_round_type==FT & raised_amount_usd>=5000000 & raised_amount_usd<=15000000 )
USA_agg<-data.frame(summarise(sector_groups,tot_fund=sum(raised_amount_usd,na.rm=T),tot_count=length(company_permalink)))
D1<-merge(D1,USA_agg,by.x="main_sector",by.y="main_sector",all.x=T)

#-- D2
sector_groups<-group_by(subset(master_frame_merged,funding_round_type==FT & country_code ==Country_2 &  raised_amount_usd>=5000000 & raised_amount_usd<=15000000),main_sector)
D2<- subset(master_frame_merged,country_code==Country_2 & funding_round_type==FT & raised_amount_usd>=5000000 & raised_amount_usd<=15000000 )
GBR_agg<-data.frame(summarise(sector_groups,tot_fund=sum(raised_amount_usd,na.rm=T),tot_count=length(company_permalink)))
D2<-merge(D2,GBR_agg,by.x="main_sector",by.y="main_sector",all.x=T)

#-- D3
sector_groups<-group_by(subset(master_frame_merged,funding_round_type==FT & country_code ==Country_3 & raised_amount_usd>=5000000 & raised_amount_usd<=15000000),main_sector)
D3<- subset(master_frame_merged,country_code==Country_3 & funding_round_type==FT & raised_amount_usd>=5000000 & raised_amount_usd<=15000000 )
IND_agg<-data.frame(summarise(sector_groups,tot_fund=sum(raised_amount_usd,na.rm=T),tot_count=length(company_permalink)))
D3<-merge(D3,IND_agg,by.x="main_sector",by.y="main_sector",all.x=T)


#Total number of Investments (count)
#===============================================
#Country_1  
nrow(D1)    #--12150

#Country_2  
nrow(D2)   #--628

#Country_3  
nrow(D3)   #--330

#Total amount of investment (USD)
#================================================
#Country_1  
sum(D1$raised_amount_usd)    #--108531347515

#Country_2  
sum(D2$raised_amount_usd)    #--5436843539

#Country_3  
sum(D3$raised_amount_usd)    #--2976543602

#Top Sector name (no. of investment-wise)
#================================================
# Excluding Blanks from the main_sector as its not part of 8 main sectors.
# Since "Others" sector is mentioned as one of the main 8 sector's, we have considered including in the top sector results

#Country_1  
unique(arrange(subset(D1,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[1]  #--"Others"

#Country_2  
unique(arrange(subset(D2,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[1]  #--"Others"

#Country_3  
unique(arrange(subset(D3,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[1]  #--"Others"



#Second Sector name (no. of investment-wise)
#================================================
#Country_1  
unique(arrange(subset(D1,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[2]   #--"Social..Finance..Analytics..Advertising"

#Country_2  
unique(arrange(subset(D2,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[2]  #--"Social..Finance..Analytics..Advertising"

#Country_3  
unique(arrange(subset(D3,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[2]  #--"Social..Finance..Analytics..Advertising"

#Third Sector name (no. of investment-wise)
#================================================
#Country_1  
unique(arrange(subset(D1,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[3]   #--"Cleantech...Semiconductors"

#Country_2  
unique(arrange(subset(D2,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[3]  #--"Cleantech...Semiconductors"

#Country_3  
unique(arrange(subset(D3,main_sector!='Blanks' ),by=desc(tot_count))$main_sector)[3]  #--"News..Search.and.Messaging"




#Number of investments in top sector (3)
#=================================================
nrow(filter(D1, main_sector=="Others")) #2950

nrow(filter(D2, main_sector=="Others")) #147

nrow(filter(D3, main_sector=="Others")) #110

#Number of investments in second sector (4)
#---------------------------------------------------
nrow(filter(D1, main_sector=="Social..Finance..Analytics..Advertising")) #2714

nrow(filter(D2, main_sector=="Social..Finance..Analytics..Advertising")) #133

nrow(filter(D3, main_sector=="Social..Finance..Analytics..Advertising")) #60

#Number of investments in third sector (5)
#---------------------------------------------------
nrow(filter(D1, main_sector=="Cleantech...Semiconductors")) #2350

nrow(filter(D2, main_sector=="Cleantech...Semiconductors")) #130

nrow(filter(D3, main_sector=="News..Search.and.Messaging")) #52


#For point 3 (top sector count-wise), which company received the highest investment?
#==========================================================================================
company_groups<-group_by(filter(D1, main_sector=="Others"),name)
arrange(data.frame(summarise(company_groups,tot_investments=sum(raised_amount_usd))),desc(tot_investments))[1,]$name

#--"Virtustream"

company_groups<-group_by(filter(D2, main_sector=="Others"),name)
arrange(data.frame(summarise(company_groups,tot_investments=sum(raised_amount_usd))),desc(tot_investments))[1,]$name

  #--"Electric Cloud"

company_groups<-group_by(filter(D3, main_sector=="Others"),name)
arrange(data.frame(summarise(company_groups,tot_investments=sum(raised_amount_usd))),desc(tot_investments))[1,]$name

#--"FirstCry.com"

#For point 4 (second best sector count-wise), which company received the highest investment?

company_groups<-group_by(filter(D1, main_sector=="Social..Finance..Analytics..Advertising"),name)
arrange(data.frame(summarise(company_groups,tot_investments=sum(raised_amount_usd))),desc(tot_investments))[1,]$name

#--"SST Inc. (Formerly ShotSpotter)"

company_groups<-group_by(filter(D2, main_sector=="Social..Finance..Analytics..Advertising"),name)
arrange(data.frame(summarise(company_groups,tot_investments=sum(raised_amount_usd))),desc(tot_investments))[1,]$name

#--"Celltick Technologies"

company_groups<-group_by(filter(D3, main_sector=="Social..Finance..Analytics..Advertising"),name)
arrange(data.frame(summarise(company_groups,tot_investments=sum(raised_amount_usd))),desc(tot_investments))[1,]$name

#--"Manthan Systems"

