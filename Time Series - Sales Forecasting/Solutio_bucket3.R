library(lubridate)
library(dplyr)

library(forecast)
library(tseries)
require(graphics)

#Reading the sales data
sales <- read.csv("Global Superstore.csv", header = T, sep = ',')
str(sales)

#getting the unique combinations of Market and Segment to split the data
#into different buckets
buckets <- unique(sales[c("Market", "Segment")])

#Initializing a list to store the CV vales for each bucket
datalist = list()

#Starting of for loop for splitting the data into buckets and to
#calculate the total profit and CVs


#bucket Creation for different regions and segment combination
for(i in 1:nrow(buckets)) { 
  condition1 <- buckets[i,1]
  condition2 <- buckets[i,2]
  data <- subset(sales, Market==condition1 & Segment==condition2)
  data$cmonth <- month(dmy(data$Order.Date))
  grp <- group_by(data, Market,Segment,cmonth)
  data <- summarise(grp, totalsales=sum(Sales), totalquantity=sum(Quantity), totalprofit=sum(Profit))
  bucketname <- paste("bucket",i,sep="_")
  assign(paste("bucket",i,sep="_"),data) 
#calculating the total profit and CV for each
  temp <- data.frame(bucket= bucketname,Tproft=sum(data$totalprofit),sd=sd(data$totalprofit),mean=mean(data$totalprofit),cv=sd(data$totalprofit)/mean(data$totalprofit))
  datalist[[i]] <- temp
}
#This table below contains all the bucketnames along with the totalprofit
#as well the standard deviation, mean and Cvs across all of them
comp.table <- do.call(rbind,datalist)

##Selecting the bucket 3 and 12 with maximum proft and least CV
#Removing all other buckets
a <- 1:21
keep <- c(3,12)
remove <- a[!a %in% keep]
for (i in remove)
{
  bucketname <- paste("bucket",i,sep="_")
  rm(list=c(bucketname))
}



##########

ylab <- c("Sales Registered")
xlab <- c("Months from Jan ")
title <- c("Sales of company ")
xcol <- c(1)
ycol <- c(2)

rawdata <- bucket_3

nrow(rawdata)

#Let's create the model using the first 6 rows.
#Then we can test the model on the remaining 6 rows later

total_timeser <- ts(log(rawdata$totalsales))
indata <- rawdata[1:6,]
timeser <- ts(log(indata$totalsales))
plot(timeser)