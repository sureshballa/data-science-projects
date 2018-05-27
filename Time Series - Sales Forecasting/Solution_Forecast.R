## Instruction to reviewer: Please make sure to set current working directory to same directory
## where all the files are present

## IIITB - Group_Facilitator_RollNo: DDA1730041
## Team:
## 1) Fayiz Mayam Veetil
## 2) Merin Jose
## 3) Deepak Aneja
## 4) Suresh Balla
################################################################################################################################################

## Business Objective: GlobalMart is online store. Find top 2 profitable accross market 
#and categoties to forecase next 6 months of sales

library(lubridate)
library(dplyr)

library(forecast)
library(tseries)
require(graphics)
require(ggplot2)


## Load data sets
sales <- read.csv("Global Superstore.csv", header = T, sep = ',')
str(sales)

## check quality checks
colSums(is.na(sales))
colMeans(is.na(sales))
barplot(colMeans(is.na(sales)))

levels(sales$Market)
# 7 markets

levels(sales$Category)
# 3 categoty

#getting the unique combinations of Market and Segment to split the data
#into different buckets
buckets <- unique(sales[c("Market", "Segment")])
nrow(buckets)
# 21 total buckets

#Initializing a list to store the CV vales for each bucket
datalist = list()

#Starting of for loop for splitting the data into buckets and to
#calculate the total profit and CVs

#bucket Creation for different regions and segment combination
for(i in 1:nrow(buckets)) 
{ 
  condition1 <- buckets[i,1]
  condition2 <- buckets[i,2]
  data <- subset(sales, Market==condition1 & Segment==condition2)
  data$cmonth <- month(dmy(data$Order.Date))
  data$cyear <- year(dmy(data$Order.Date))
  grp <- group_by(data, Market,Segment,cyear, cmonth)
  data <- summarise(grp, totalsales=sum(Sales), totalquantity=sum(Quantity), totalprofit=sum(Profit))
  bucketname <- paste("bucket",i,sep="_")
  assign(paste("bucket",i,sep="_"),data) 
  #calculating the total profit and CV for each
  temp <- data.frame(bucket= bucketname,Tproft=sum(data$totalprofit),sd=sd(data$totalprofit),mean=mean(data$totalprofit),cv=sd(data$totalprofit)/mean(data$totalprofit))
  datalist[[i]] <- temp
}

#This table below contains all the bucketnames along with the totalprofit
#as well the standard deviation, mean and Cvs across all of them
comp.table <- do.call(rbind, datalist)
comp.table

#Outcome
##########################################################################################
####Selecting the bucket 12 and 3 as per least cv and maximum proft
##########################################################################################

#Modeling doing using 1. Classic decomposition, 2. ARIMA

########## starting analysis on bukcet 12 #############
 
rawdata <- bucket_12
nrow(rawdata)

#Let's Visualise the time series
total_timeser <- ts(rawdata$totalsales)

#Let's create the model using the first 42 rows for 42 months of data 
#Last 6 months data would be used for testing the model. This is called intrinsic   
indata <- rawdata[1:42,]

timeser <- ts(indata$totalsales)

ylab <- c("Sales Registered")
xlab <- c("Months")
title <- c("Sales of company: Period 2011 - 2014 ")

plot(timeser,main=title, xlab = xlab, ylab = ylab) #Plot: time searies
#Outcome: plot suggest that it has Upward trenda and seasonality  

#Smoothing the series - Moving Average Smoothing

w <- .5
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
timevals_in <- seq(1:length(indata$cmonth))
lines(smoothedseries, col="red", lwd=2)
#plot: time earies with smoothening line

###### Building a model on the smoothed time series using classical decomposition

#First let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('cmonth', 'totalsales')

#Now, let's fit a regression model for trend and seasonality to the data
lmfit <- lm(totalsales ~ cmonth, data=smootheddf)
global_pred <- predict(lmfit, cmonth=timevals_in)
global_pred
summary(global_pred)

#let's add the global pred line on top of smoothing line
lines(timevals_in, global_pred, col='green', lwd=2)

#let's plot the global pred regression line individually
plot(global_pred, col='red', type = "l")
#plot: global pred regression line 

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser - global_pred
plot(local_pred, col='red', type = "l")
#plot: Local predicition 

#let's test local for Stationary  
acf(local_pred, type="partial") #PACF for optimal value of p
acf(local_pred) #ACF for optimal value of q
#outcome: ACF and PACF plots indicate that is a strong stationarity (white noise only) 
#because plot shows that tvalues are not significantly different from zero for non-zero lags

armafit <- auto.arima(local_pred)

tsdiag(armafit)

armafit
#ARIMA(0,0,0)
#Outcome: There is no auto regressive so no local dependency. 
#It also mean that there is strong stationarity (white noise only)   

#We'll check if the residual series is white noise
resi <- local_pred - fitted(armafit)

#let's test local for Stationary  
acf(local_pred, type="partial") #PACF for optimal value of p
acf(local_pred) #ACF for optimal value of q
#outcome: ACF and PACF plots indicate that is a strong stationarity (white noise only) 
#because plot shows that tvalues are not significantly different from zero for non-zero lags

#Null hypothesis assumes that the series is not stationary
adf.test(resi,alternative = "stationary")
#p value is 0.03
#i.e. reject the hypothesis which means residual is  stationary 

#Null hypothesis assumes that the series is stationary
kpss.test(resi)
#p value .1
#fail to reject the hypothesis which means TS stationary


#Outcome: Residue is white noise only

#First, let's make a prediction for the last 6 months
indata <- rawdata[1:42,]
outdata <- rawdata[43:48,]
timevals_out <- c(43:48) 

global_pred_out <- predict(lmfit,data.frame(cmonth =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$totalsales)[5]
MAPE_class_dec
#Outcome: Mean Absolute percentage error 28.15%

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
#plot: forecast with classical decomposition

#end of analysis and forecasting using classical decomposition

#start analysis and forecasting using ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
#plot: time series with ARIMA smoothening  

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

#residue after ARIMA fitting 
plot(resi_auto_arima)
#Plot: residue after ARIMA fitting

#let's test local for Stationary  
acf(resi_auto_arima, type="partial") #PACF for optimal value of p
acf(resi_auto_arima) #ACF for optimal value of q
#outcome: ACF and PACF plots indicate that is a strong stationarity (white noise only) 
#because plot shows that tvalues are not significantly different from zero for non-zero lags

#Null hypothesis assumes that the series is not stationary
adf.test(resi_auto_arima,alternative = "stationary")
#p value is 0.01
#i.e. reject the hypothesis which means residual is  stationary 

#Null hypothesis assumes that the series is stationary
kpss.test(resi_auto_arima)
#p value .1
#fail to reject the hypothesis which means TS stationary

#Outcome: After ARIMA fitting, the residue is white noise only

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$totalsales)[5]
MAPE_auto_arima
#Mean Absolute percentage using ARIMA 28.92%

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")
#plot: forecast with ARIMA

##########end of modeling for Bucket 12############################

########## starting analysis on bukcet 3 #############

rawdata <- bucket_3
nrow(rawdata)

#Let's Visualise the time series
total_timeser <- ts(rawdata$totalsales)

#Let's create the model using the first 42 rows for 42 months of data 
#Last 6 months data would be used for testing the model. This is called intrinsic   
indata <- rawdata[1:42,]

timeser <- ts(indata$totalsales)

ylab <- c("Sales Registered")
xlab <- c("Months")
title <- c("Sales of company - EU:Consumer :: Period 2011 - 2014 ")

plot(timeser,main=title, xlab = xlab, ylab = ylab) #Plot: time searies
#Outcome: plot suggest that it has Upward trenda and seasonality  

#Smoothing the series - Moving Average Smoothing

w <- .5
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
timevals_in <- seq(1:length(indata$cmonth))
lines(smoothedseries, col="red", lwd=2)
#plot: time earies with smoothening line

###### Building a model on the smoothed time series using classical decomposition

#First let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('cmonth', 'totalsales')

#Now, let's fit a regression model for trend and seasonality to the data
lmfit <- lm(totalsales ~ cmonth, data=smootheddf)
global_pred <- predict(lmfit, cmonth=timevals_in)
global_pred
summary(global_pred)

#let's add the global pred line on top of smoothing line
lines(timevals_in, global_pred, col='green', lwd=2)

#let's plot the global pred regression line individually
plot(global_pred, col='red', type = "l")
#plot: global pred regression line 

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser - global_pred
plot(local_pred, col='red', type = "l")
#plot: Local predicition 

#let's test local for Stationary  
acf(local_pred, type="partial") #PACF for optimal value of p
acf(local_pred) #ACF for optimal value of q
#outcome: ACF and PACF plots indicate that is a strong stationarity (white noise only) 
#because plot shows that tvalues are not significantly different from zero for non-zero lags

armafit <- auto.arima(local_pred)

tsdiag(armafit)

armafit
#ARIMA(0,0,0)
#Outcome: There is no auto regressive so no local dependency. 
#It also mean that there is strong stationarity (white noise only)   

#We'll check if the residual series is white noise
resi <- local_pred - fitted(armafit)

#let's test local for Stationary  
acf(local_pred, type="partial") #PACF for optimal value of p
acf(local_pred) #ACF for optimal value of q
#outcome: ACF and PACF plots indicate that is a strong stationarity (white noise only) 
#because plot shows that tvalues are not significantly different from zero for non-zero lags

#Null hypothesis assumes that the series is not stationary
adf.test(resi,alternative = "stationary")
#p value is 0.015
#i.e. reject the hypothesis which means residual is  stationary 

#Null hypothesis assumes that the series is stationary
kpss.test(resi)
#p value .1
#fail to reject the hypothesis which means TS stationary


#Outcome: Residue is white noise only

#First, let's make a prediction for the last 6 months
indata <- rawdata[1:42,]
outdata <- rawdata[43:48,]
timevals_out <- c(43:48) 

global_pred_out <- predict(lmfit,data.frame(cmonth =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$totalsales)[5]
MAPE_class_dec
#Outcome: Mean Absolute percentage error 25.16%

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
#plot: forecast with classical decomposition

#end of analysis and forecasting using classical decomposition

#start analysis and forecasting using ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
#plot: time series with ARIMA smoothening  

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

#residue after ARIMA fitting 
plot(resi_auto_arima)
#Plot: residue after ARIMA fitting

#let's test local for Stationary  
acf(resi_auto_arima, type="partial") #PACF for optimal value of p
acf(resi_auto_arima) #ACF for optimal value of q
#outcome: ACF and PACF plots indicate that is a strong stationarity (white noise only) 
#because plot shows that tvalues are not significantly different from zero for non-zero lags

#Null hypothesis assumes that the series is not stationary
adf.test(resi_auto_arima,alternative = "stationary")
#p value is 0.01
#i.e. reject the hypothesis which means residual is  stationary 

#Null hypothesis assumes that the series is stationary
kpss.test(resi_auto_arima)
#p value .1
#fail to reject the hypothesis which means TS stationary

#Outcome: After ARIMA fitting, the residue is white noise only

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$totalsales)[5]
MAPE_auto_arima
#Mean Absolute percentage using ARIMA 27.69%

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")
#plot: forecast with ARIMA

##########end of modeling for Bucket 3
