###############################################################################
# SYNOPSIS: Implements a Logistic Regression Model on airline delays dataset
# DESCRIPTION : The data used is from the below link.We use 2007 data to 
# predict Departure delays for flights originating from JFK Airport
# Ref Links : http://stat-computing.org/dataexpo/2009/the-data.html
# PARAMETERS : NONE
# OUTPUTS : LInear Regression Model
# 
# Version History
# ================
# v0.1 - 16-Apr-2015 - Pavan Keerthi - Initial Version


###############################################################################

# Load required libraries.
## Here order is important since these packages mask common functions
library(Hmisc)
library(dplyr) 
library(stringr)
library(reshape2)
library(ggplot2)
library(glm2)


# set working directory
#Note: Change this to your local path
setwd("C:\\Users\\Pavan\\Work\\LearnML\\LogisticRegression\\R-Implementation")





# Load CSV data
data<-read.csv("C:\\Users\\Pavan\\Work\\LearnML\\data\\airlinedelays\\2007.csv",header=TRUE,sep=",",na.strings = "NA",stringsAsFactors =TRUE)


# We are only interested to build model for Flights from JFK.
data <-filter(data,Origin =='JFK')


# Read Structure
str(data)


# convert int columns as factor where data is categorical
## Period Columns
data$Der_Day<-as.factor(data$DayofMonth)
data$Der_Weekday<-as.factor(data$DayOfWeek)
data$Der_Month<-as.factor(data$Month)
data$Der_Year<-as.factor(data$Year)  

## Other Columns
data$Der_FlightNum<- as.factor(data$FlightNum)
data$Der_Cancelled<-as.factor(data$Cancelled)
data$Der_Diverted<-as.factor(data$Diverted)

# Read Structure
str(data)

# Sense Check how Diverted Flights affect DepDelays
summary(select(filter(data,Der_Diverted==1),DepDelay))

####Answer:Useful.Retain Diverted flights for analysis

# Sense Check how Cancelled Flights affect DepDelays
summary(select(filter(data,Der_Cancelled==1),DepDelay))

####Answer:All are NA's.Filter them out from datasets

# Retain only Not Cancelled flights
data <-filter(data,Cancelled==0)

# Sense check why ArrTime < DepTime
nrow(filter(data,ArrTime<DepTime))
head(select(filter(data,ArrTime<DepTime),DayofMonth,Month,Year,FlightNum,Origin,Dest,Distance,DepTime,ArrTime),n=10)
tail(select(filter(data,ArrTime<DepTime),DayofMonth,Month,Year,FlightNum,Origin,Dest,Distance,DepTime,ArrTime),n=10)


#####Answer: Destination Arrival Time adjusting for Timezones

# Create DateTime Columns from original data

##Convert date parts variables to string
data <-mutate(data,
              Der_str_Day=as.character(DayofMonth),
              Der_str_Month=as.character(Month),
              Der_str_Year=as.character(Year),
              Der_str_DepTime=as.character(DepTime),
              Der_str_ArrTime=as.character(ArrTime),
              Der_str_CRSDepTime=as.character(CRSDepTime),
              Der_str_CRSArrTime=as.character(CRSArrTime)              
)

### Sense check data
head(select(data,DepTime,Der_str_DepTime),n=10)

## format time strings to add leading 0
data <- mutate(data,
               Der_frmt_DepTime = str_replace_all(Der_str_DepTime,str_sub(Der_str_DepTime,start=-2),paste(":",str_sub(Der_str_DepTime,start=-2),sep="")),
               Der_frmt_ArrTime = str_replace_all(Der_str_ArrTime,str_sub(Der_str_ArrTime,start=-2),paste(":",str_sub(Der_str_ArrTime,start=-2),sep="")),
               Der_frmt_CRSDepTime = str_replace_all(Der_str_CRSDepTime,str_sub(Der_str_CRSDepTime,start=-2),paste(":",str_sub(Der_str_CRSDepTime,start=-2),sep="")),
               Der_frmt_CRSArrTime = str_replace_all(Der_str_CRSArrTime,str_sub(Der_str_CRSArrTime,start=-2),paste(":",str_sub(Der_str_CRSArrTime,start=-2),sep=""))
)

## Some rows will be of format :[H]H:MM .Correct them to be [H]H:MM
data <- mutate(data,
               Der_fmrt_DepTime = str_replace(Der_frmt_DepTime,":",""),
               Der_fmrt_ArrTime = str_replace(Der_frmt_ArrTime,":",""),
               Der_fmrt_CRSDepTime = str_replace(Der_frmt_CRSDepTime,":",""),
               Der_fmrt_CRSArrTime = str_replace(Der_frmt_CRSArrTime,":","")
)


## Sense check new column formats

head(select(data,DepTime,Der_str_DepTime,Der_frmt_DepTime,Der_str_ArrTime,Der_frmt_ArrTime),n=10)
tail(select(data,DepTime,Der_str_DepTime,Der_frmt_DepTime,Der_str_ArrTime,Der_frmt_ArrTime),n=10)

head(select(data,CRSDepTime,Der_str_CRSDepTime,Der_frmt_CRSDepTime,Der_str_CRSArrTime,Der_frmt_CRSArrTime),n=10)
tail(select(data,CRSDepTime,Der_str_CRSDepTime,Der_frmt_CRSDepTime,Der_str_CRSArrTime,Der_frmt_CRSArrTime),n=10)


## Add DateTime Columns for ActualDep,ActualArr,ScheduledDep,ScheduledArr

### JFK is in EST timezone ,but we dont know timezone of Dest 
### To keep it simple now,just base everything to UTC.

### Check Sample first
as.POSIXct(paste(data$Der_str_Year[2],"-",data$Der_str_Month[2],"-",data$Der_str_Day[2]," ",data$Der_frmt_DepTime[2],sep=""),format="%Y-%m-%d %H:%M", tz = "UTC")



## Now COnvert all rows
data<-mutate(data,
             Der_DepDateTime=as.POSIXct(paste(Der_str_Year,"-",Der_str_Month,"-",Der_str_Day," ",Der_frmt_DepTime,sep=""),format="%Y-%m-%d %H:%M", tz = "UTC"),
             Der_ArrDateTime=as.POSIXct(paste(Der_str_Year,"-",Der_str_Month,"-",Der_str_Day," ",Der_frmt_ArrTime,sep=""),format="%Y-%m-%d %H:%M", tz = "UTC"),
             Der_CRSDepDateTime=as.POSIXct(paste(Der_str_Year,"-",Der_str_Month,"-",Der_str_Day," ",Der_frmt_CRSDepTime,sep=""),format="%Y-%m-%d %H:%M", tz = "UTC"),
             Der_CRSArrDateTime=as.POSIXct(paste(Der_str_Year,"-",Der_str_Month,"-",Der_str_Day," ",Der_frmt_CRSArrTime,sep=""),format="%Y-%m-%d %H:%M", tz = "UTC")
)


## Sense check DateTimeColumns

head(select(data,Der_frmt_DepTime,Der_DepDateTime,Der_frmt_ArrTime,Der_ArrDateTime),n=10)
tail(select(data,Der_frmt_DepTime,Der_DepDateTime,Der_frmt_ArrTime,Der_ArrDateTime),n=10)

head(select(data,Der_frmt_CRSDepTime,Der_CRSDepDateTime,Der_frmt_CRSArrTime,Der_CRSArrDateTime),n=10)
tail(select(data,Der_frmt_CRSDepTime,Der_CRSDepDateTime,Der_frmt_CRSArrTime,Der_CRSArrDateTime),n=10)


### How many Rows doesnt have valid Dep date time
nrow(filter(data,is.na(Der_DepDateTime)))

## filter out invalid date rows for now. 
#### TODO: do RCA later
data <- mutate(data, TempId =seq.int(nrow(data)))
badrows<- as.double((select(filter(data,is.na(Der_DepDateTime)),TempId))$TempId)
data<-select(data[-badrows,],-matches("TempId"))
#### cleanup temp objects
rm(badrows)

# Sense check delay data.We are only interested in Departure Delays
head(select(data,DepDelay),n=10)
tail(select(data,DepDelay),n=10)

## See distribution of DepDelay
summary(data$DepDelay)
var(data$DepDelay)
sd(data$DepDelay)


# We want to consider a Departure delay only if its more than 15 mins delayed
## Add Binary flag to indicate Departure delay
data <- mutate(
  data,
  Der_DepDelayed =  DepDelay>15
)

## Check data
head(select(data,DepDelay,Der_DepDelayed),n=5)
tail(select(data,DepDelay,Der_DepDelayed),n=5)

tabulate(as.factor(data$Der_DepDelayed))

# Add Identity Coulmn to data frame
data <- mutate(data, Id =seq.int(nrow(data)))

# See final structure
str(data)

# Plot Correlations 

## Delays by Month
MonthlyDelays <-summarise(group_by(
  select(data,Der_Month,Der_DepDelayed),Der_Month
),mean_delays=mean(Der_DepDelayed))
MonthlyDelays$Der_Month = as.factor(MonthlyDelays$Der_Month)


Mgraph <- ggplot(MonthlyDelays)+
  geom_bar(aes(x=Der_Month,y=mean_delays),stat="identity",width=0.5)+
  labs(title="Flight Delays by Month",x="Month",y="Mean Delay")
plot(Mgraph)

ggsave(file="Delays_by_Month.jpg",Mgraph)

##remove temp objects
rm(MonthlyDelays)
rm(Mgraph)



## Delays by Hour

### First derive hour from hh:mm
HourlyDelays <- select(data,Der_frmt_DepTime,Der_DepDelayed) 

HourlyDelays <- mutate(HourlyDelays, 
                       Der_Hour=str_sub(Der_frmt_DepTime,start=1,end=2))

HourlyDelays <- mutate(HourlyDelays, 
                       Der_Hour=str_replace(Der_Hour,':',''))



#### summarise data
HourlyDelays <-summarise(group_by(
  select(HourlyDelays,Der_Hour,Der_DepDelayed),Der_Hour
),mean_delays=mean(Der_DepDelayed)
)

### Convert Hour to int and sort the order
HourlyDelays$Der_Hour <- as.integer(HourlyDelays$Der_Hour)
HourlyDelays<-HourlyDelays[order(HourlyDelays$Der_Hour),]


Hgraph <- ggplot(HourlyDelays)+
  geom_bar(aes(x=Der_Hour,y=mean_delays),stat="identity",width=0.5)+
  labs(title="Flight Delays by Hour",x="Hour",y="Mean Delay")+
  scale_x_continuous( breaks=c(1:24))

plot(Hgraph)

ggsave(file="Delays_by_Hour.jpg",Hgraph)

##remove temp objects
rm(HourlyDelays)
rm(Hgraph)


## Delays by Carriers
CarrierDelays <-summarise(group_by(
  select(data,UniqueCarrier,Der_DepDelayed),UniqueCarrier
),mean_delays=mean(Der_DepDelayed))

Cgraph <- ggplot(CarrierDelays)+
  geom_bar(aes(x=UniqueCarrier,y=mean_delays),stat="identity",width=0.5)+
  labs(title="Flight Delays by Carrier",x="Carrier",y="Mean Delay")

plot(Cgraph)

ggsave(file="Delays_by_Carrier.jpg",Cgraph)

## remove temp objects
rm(CarrierDelays)
rm(Cgraph)


## Delays by Distance

DistanceDelays <- mutate(data,
                         Der_DistanceBand=cut2(data$Distance,g=5))
DistanceDelays <-summarise(group_by(
  select(DistanceDelays,Der_DistanceBand,Der_DepDelayed),Der_DistanceBand
),mean_delays=mean(Der_DepDelayed))

Dgraph <- ggplot(DistanceDelays)+
  geom_bar(aes(x=Der_DistanceBand,y=mean_delays),stat="identity",width=0.5)+
  labs(title="Flight Delays by Distance",x="Distance",y="Mean Delay")

plot(Dgraph) 

ggsave(file="Delays_by_DistanceRange.jpg",Dgraph)

## remove temp objects
rm(DistanceDelays)
rm(Dgraph)

## Delays by Destination

DestDelays <-summarise(group_by(
  select(data,Dest,Der_DepDelayed),Dest
),mean_delays=mean(Der_DepDelayed))

### Focus atleats MeanDelay is atleast 30%
DestDelays <-filter(DestDelays,mean_delays>=0.30)

Destgraph <- ggplot(DestDelays )+
  geom_bar(aes(x=Dest,y=mean_delays),stat="identity",width=0.5)+
  labs(title=expression(atop("Flight Dleays by Destinations", atop(italic("Total Delays atleast 30%"), ""))),sub="Destinations where delays are atleast 30%",x="Destination",y="Mean Delay")+
  theme(axis.text.x=element_text(angle=90))

plot(Destgraph) 

ggsave(file="Delays_by_Destination.jpg",Destgraph)


## remove temp objects
rm(DestDelays)
rm(Destgraph)

# Create Train /Test sets

## Find subset size
samp_size<-nrow(data)/2

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = samp_size,replace=FALSE)

## prepare Training and Test set
traindata <- data[train_ind,]
testdata <- data[-train_ind,]

### cleanup temp objects
rm(samp_size)
rm(train_ind)


# Start working with training set

## Create a feature matrix

trainfeatures<-select(
  mutate(select(traindata,Id,Der_Month,Der_frmt_DepTime,UniqueCarrier,Distance,Dest,Der_DepDelayed),
         Month=Der_Month,
         Hour=as.factor(str_replace(str_sub(Der_frmt_DepTime,start=1,end=2),':','')),
         Carrier=UniqueCarrier,
         Destination=Dest,
         IsDelayed=Der_DepDelayed
  )
  ,Id,Month,Hour,Carrier,Distance,Destination,IsDelayed)

# Add Distance Ranges as quartiles
trainfeatures$DistanceRange <- cut2(trainfeatures$Distance,g=5)

#structure of feature
str(trainfeatures)

#Drop data ,since we can always recreate  from train and test sets
rm(data)


#Train Logistic Regression Model
#We are using only 2 variables out of potential 5 due to PC memory/cpu restrictions
formula = "IsDelayed ~ Month * Hour"

glm.logit= glm(formula, family=binomial(logit), data=trainfeatures)

summary(glm.logit)


#Create Test feature matrix
testfeatures<-select(
  mutate(select(testdata,Id,Der_Month,Der_frmt_DepTime,UniqueCarrier,Distance,Dest,Der_DepDelayed),
         Month=Der_Month,
         Hour=as.factor(str_replace(str_sub(Der_frmt_DepTime,start=1,end=2),':','')),
         Carrier=UniqueCarrier,
         Destination=Dest,
         IsDelayed=Der_DepDelayed
  )
  ,Id,Month,Hour,Carrier,Distance,Destination,IsDelayed)

# Add Distance Ranges as quartiles
testfeatures$DistanceRange <- cut2(testfeatures$Distance,g=5)


#Predict test set using glm.logit model
predict <- predict(glm.logit,newdata=testfeatures,type="response")


# Function to compute Precision, Recall and F1-Measure
get_metrics <- function(predicted, actual) {
  tp = length(which(predicted == TRUE & actual == TRUE))
  tn = length(which(predicted == FALSE & actual == FALSE))
  fp = length(which(predicted == TRUE & actual == FALSE))
  fn = length(which(predicted == FALSE & actual == TRUE))
  
  precision = tp / (tp+fp)
  recall = tp / (tp+fn)
  F1 = 2*precision*recall / (precision+recall)
  accuracy = (tp+tn) / (tp+tn+fp+fn)
  
  v = c(precision, recall, F1, accuracy)
  v
}

metrics = get_metrics(predict>= 0.5,as.logical(testfeatures$IsDelayed))


print(sprintf("Logistic Regression Model1: precision=%0.2f, recall=%0.2f, F1=%0.2f, accuracy=%0.2f", 
              metrics[1], metrics[2], metrics[3], metrics[4]))
