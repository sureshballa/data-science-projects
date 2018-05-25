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

rawdata <- bucket_12

nrow(rawdata)

#Let's create the model using the first 6 rows.
#Then we can test the model on the remaining 6 rows later

total_timeser <- ts(log(rawdata$totalsales))
indata <- rawdata[1:6,]
timeser <- ts(log(indata$totalsales))
plot(timeser)


#Smoothing the series - Moving Average Smoothing

w <-0.5
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$cmonth
lines(smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('cmonth', 'totalsales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
#            + Month, data=smootheddf)

#

lmfit <- lm(totalsales ~ cmonth, data=smootheddf)
global_pred <- predict(lmfit, cmonth=timevals_in)
global_pred
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- rawdata[7:12,]
timevals_out <- outdata$cmonth
outdata$totalsales<-log(outdata$totalsales)

global_pred_out <- predict(lmfit,data.frame(cmonth =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$totalsales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$totalsales)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")