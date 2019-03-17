library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)


################################################################################################
####################          Metadata of Uber data            #################################

## 1. Data frame Contains 6745 observations
## 2. Column Driver.id has 2650 values when customer are ended up with "No Cars available" 
##    and Drop.timestamp has NA value, which is appropriate as there is customer drop happened.
## 3. Drop.timestamp column has total of 3914 NA values. 
##    The reason being when driver initiated cancel of the trip and no cars available for drop.

################################################################################################


################################################################################################
                    ##### Cleansing the data before analysis   #########
################################################################################################


## Read the uber data from CSV into data frame

uber<-read.csv("UberRequestData.csv",stringsAsFactors = FALSE)

## Values in "Request.timestamp" column doesn't have seconds include in the format. Hence correcting the value to include seconds
## For ease of merging columns converting column "Request.timestamp" to character 

uber$Request.TS_NOSEC<-as.POSIXlt(uber$Request.timestamp, format = "%d/%m/%Y %H:%M")
uber$Request.TS_NOSEC<-as.character(uber$Request.TS_NOSEC)
uber$Request.TS_WITHSEC<-as.POSIXlt(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber$Request.TS_WITHSEC<-as.character(uber$Request.TS_WITHSEC)


## Merging temporary columns and updating the columns "Request.timestamp" to includes Seconds(H:M:S)
uber$Request.timestamp <- ifelse(!is.na(uber$Request.TS_NOSEC), uber$Request.TS_NOSEC,uber$Request.TS_WITHSEC)
uber$Request.timestamp<-as.POSIXct(uber$Request.timestamp,format="%Y-%m-%d %H:%M:%S")

## Removing temporary columns which are derived from "Request.timestamp" 
uber<-uber[,c(-7,-8)] #Remove temporary columns used for cleaning "Request.timestamp" column

## Few Values in "Drop.timestamp" column doesn't have seconds include in the format. Hence correcting the value to include seconds
## For ease of merging columns converting column "Drop.timestamp" to character 

uber$Drop.TS_NOSEC<-as.POSIXlt(uber$Drop.timestamp, format = "%d/%m/%Y %H:%M")
uber$Drop.TS_NOSEC<-as.character(uber$Drop.TS_NOSEC)
uber$Drop.TS_WITHSEC<-as.POSIXlt(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber$Drop.TS_WITHSEC<-as.character(uber$Drop.TS_WITHSEC)

## Merging temporary columns and updating the columns "Drop.timestamp" to includes Seconds(H:M:S)
uber$Drop.timestamp<- ifelse(!is.na(uber$Drop.TS_NOSEC), uber$Drop.TS_NOSEC,uber$Drop.TS_WITHSEC)
uber$Drop.timestamp<-as.POSIXct(uber$Drop.timestamp,format="%Y-%m-%d %H:%M:%S")

## Removing temporary columns which are derived from "Drop.timestamp"
uber<-uber[,c(-7,-8)]

## Deriving new columns Date,Time, and hours from Request.timestamp for later analysis
uber$Req.Date<-as.Date(uber$Request.timestamp)
uber$Req.time<-format(uber$Request.timestamp,"%H:%M:%S")
uber$Req.hour<-format(uber$Request.timestamp,"%H")

################################################################################################
      ##### Visual representation of most pressing problems for Uber using R   #########

# 1. The frequency of "cancellation by drivers" and "No cars available" 
# 2. Identify the most problematic types of requests (city to airport/airport to city)
# 3. Identify the time slots (early mornings, late evenings etc.) using plots
################################################################################################

# 1. The frequency of "cancellation by drivers" and "No cars available"
## The frequency of "cancellation by drivers" and "No cars available" 
## can be identified through Status column which is categorical in nature.
## Applying univariate on categorical data by drawing histogram. The results of histogram excludes status with "Trip Completed".

plot_freq<-ggplot(subset(uber,uber$Status!="Trip Completed"),aes(x=Status,fill=Status)) + 
  geom_histogram(stat = "count",width=0.4) +
  labs(title="Frequency of Uber Request Status",x="Request Status",y="Number of Requests") +
geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  theme(axis.title = element_text(color="#666666", face="bold", size=12)) 

plot_freq


#######################################################################################################################
          # Considering only Cancelled and "No Cars available"
          # Results shows 68% customers trips were affected by "No Cars available" and 
          # 32.3% were impacted as their trips were cancelled. 
#######################################################################################################################

# 2.Draw histogram to identify the pattern of pickup 

plot_freq_pickup<-ggplot(uber,aes(x=Pickup.point,fill=Status)) + 
  scale_fill_brewer(palette = "Set1") +
   geom_histogram(stat="count",position="dodge",width=0.7) + 
  labs(title="Most problematic Request Type in Airport and City",x="Pickup from",y="Customer Request Status",subtitle="\"Cancelled\" and \"No cars Available\" are significant problems") + 
  theme(axis.title = element_text(color="#666666", face="bold", size=12)) 
plot_freq_pickup

#######################################################################################################################
          # The results shows, customer bookings had impact due to status "No Cars available" by 39.3%, and cancelled by 18.7%
          # Data indicates Uber has challenge to meet the demands when customers are 
          # requesting to book a cab from airport to city and vice versa.
#######################################################################################################################

# 3. Identify the time slots (early mornings, late evenings etc.) using plots

## After identifying most pressing problems delving futher to identify
## 1. Understand the pattern of pickup points to identify peak hours 
## 2. understand the pattern of requests type and how many were impacted due to "No cars available and Cancelled"

plot_hr_pickup<-ggplot(uber,aes(x=Req.hour,fill=uber$Pickup.point)) +
  geom_histogram(stat="count",position="dodge") +
  labs(title="Inflow of customer requests in hours",x="Request Hours",y="Customer Requests",subtitle="Morning 5AM-9AM and Evening 5PM-10PM are problematic") +
  theme(axis.title = element_text(color="#666666", face="bold", size=12)) +
  scale_fill_manual(values=c("#D4E157", "#FF8A65"),name="Pickup Point",labels=c("Airport to City","City to Airport"))

plot_hr_Status<-ggplot(uber, aes(x = Req.hour, fill = Status)) +
  geom_histogram(stat="count") +
  scale_fill_brewer(palette = "Set1") +
  labs(title="Status of Customer Request",x="Request Hours",y="Customer Requests",subtitle="Morning 5AM-9AM has more \"Cancelled\" and Evening 5PM-10PM shows \"No cars Available\"") +
  theme(axis.title = element_text(color="#666666", face="bold", size=12))

grid.arrange(plot_hr_pickup,plot_hr_Status,nrow=2,ncol=1)

#######################################################################################################################
          # Results
          # The results shows two graphs, consists of frequency of Pick Points and status of cab requests
      #Graph1:
          # 1. Peak hours of cab request are during morning hours between 5AM up to 9 AM, from city to airport 
          # 2. Similarly, evening peaks are bewteen 5PM to 10PM, where customer tries to book for cab from 
          #    Airport to city.
      #Graph2:
          # 1. Morning hours between 5AM-9AM, shows increased number of "cancellation" up to 31% from city to airport
          # 2. Evening hours between 5PM-10PM,shows "no cars available" when customers tries to book cab from airport.
#######################################################################################################################


################################################################################################
    ##### Find out the gap between supply and demand and show the same using plots   #########

        # 1. Find the time slots when the highest gap exists 
        # 2. Identify the most problematic types of requests (city to airport/airport to city)
################################################################################################
# Find out the gap between supply and demand and show the same using plots.
# Create time slots for further analysis, 24 hours are create with bins Early Morning, Morning Peak, Day_time, Evening_peak and Late night


uber$Req.hour<-as.numeric(uber$Req.hour)

uber$time_slot = ifelse(uber$Req.hour < 5, "Early_Morning", 
                        ifelse(uber$Req.hour < 10,"Morning_Peak",
                 ifelse(uber$Req.hour < 17,"Day_Time",
                        ifelse(uber$Req.hour < 22,"Evening_Peak","Late_Night"))))

# This plots helps in visualizing number of peak requests on which time slot from the pick points which helps to identify which are the peak timeslots from pickup points
plot_time_pickups<-ggplot(uber, aes(x = time_slot, fill= factor(Pickup.point))) +
  geom_histogram(stat = "count",position = "dodge",width=0.5) +
  theme(axis.title = element_text(color="#666666", face="bold", size=12))+
  labs(title="1. Requests based on pickup points",x="Time Slots",y="Customer Requests",subtitle="Highest Requests are from City during Morning_peak hours and Airport in the Evening_peak hours")+
  scale_fill_manual(values=c("#D4E157", "#FF8A65"),name="Pickup Point",labels=c("Airport to City","City to Airport"))

# This graph helps to identify the highest gaps based on request types.

# Evening peaks hours are impacted by "No Cars available" and morning_peak hours are impacted by "Cancellation" when compared to other time slots
plot_time_Status<-ggplot(subset(uber,uber$Status!="Trip Completed"), aes(x = time_slot, fill= Status)) +
  geom_histogram(stat = "count",position = "dodge",width=0.5) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.title = element_text(color="#666666", face="bold", size=12))+
  labs(title="2. Determining highest gap through time slots ",x="Time Slots",y="Customer Requests",subtitle="In the Evening_peak hours due to \"No Cars Available\" and Morning_peak hours due to \"Cancellation\"")

# This plot helps to understand determining the supply/demand gap for request from City to Airport during morning peaks hours.
# During morning_peak city to airport Uber travellers experience higher number of cancellation compared to other requests and timeslots
# This has an impact on Uber revenue 
plot_time_city<- ggplot(subset(uber,(uber$Pickup.point=="City") & (uber$Status!="Trip Completed")) , aes(x = time_slot, fill = Status)) +
  geom_histogram(stat = "count",position = "dodge",width=0.5) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.title = element_text(color="#666666", face="bold", size=12))+
  labs(title="3. Determining Supply/Demand gap from City to Airport",x="Time Slots",y="Customer Requests",subtitle="\"Cancellation\" in the morning peak hours is a gap")

# This plot helps to understand determining the supply/demand gap for uber customers request from Airport to City during Evening peaks hours.
# During Evening_peak hours, airport to city Uber travellers has problem in booking cabs due to "No Cars Available".
# This has an impact on Uber revenue 
plot_time_Airport<- ggplot(subset(uber,(uber$Pickup.point=="Airport") & (uber$Status!="Trip Completed")), aes(x = time_slot, fill = Status)) +
  geom_histogram(stat = "count",position = "dodge",width=0.5) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.title = element_text(color="#666666", face="bold", size=12))+
  labs(title="4. Determining Supply/Demand gap from Airport to City",x="Time Slots",y="Customer Requests",subtitle="\"No Cars Available\" in the evening peak hours is a gap")


grid.arrange(plot_time_pickups,plot_time_Status,nrow=1,ncol=2)

grid.arrange(plot_time_city,plot_time_Airport,nrow=1,ncol=2)

# The result represents, two issue for Uber 
# 1. Customers who are trying to book cabs during Early morning hours from City to Airport significantly face cancellation
# 2. During evening peak hours, customer who are trying to book cabs from airport to city has issue as there are no cars available.
# The results shows supply of cabs to airport are not adequate during evening peak hours and demands are spiked during early_morning hours 
# from city to airport.Explanation are mentioned in the presentation.









