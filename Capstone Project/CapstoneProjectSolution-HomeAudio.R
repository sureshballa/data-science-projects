## Instruction to reviewer: Please make sure to set current working directory to same directory
## where all the files are present

## IIITB - Group_Facilitator_RollNo: DDA1730041
## Team:
## 1) Fayiz Mayam Veetil
## 2) Merin Jose
## 3) Deepak Aneja
## 4) Suresh Balla
################################################################################################################################################

## Business Objective
## TODO: Place verbiage from assignment page

## Begin of Install and load required libraries

load.libraries <- c('reshape', 'stringr', 'dplyr', 'data.table', 'e1071', 'gridExtra', 'corrplot', 'ggplot2', 'tidyr', 'MASS', 'car', 'caret', 'GGally', 'mice','cowplot','caTools')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

## End of Install and load required libraries
################################################################################################################################################


## Load data sets
#DA Handed the \N for Null values
consumerElectronicsData <- read.csv("ConsumerElectronics.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("\\N", "NA","NaN","","#DIV/0!"))
budgetAllocations <- read.csv("budget_allocation.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))

#DA Load Sales Event and NPS data
salesEventsWeeklyLevel <- read.csv("events_salesdays.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
npsWeeklyLevel <- read.csv("NPS.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))

colnames(budgetAllocations)[1] <- "Year"

## End of load data sets
################################################################################################################################################

## Reusable function to plot in grid for given configuration of number of columns
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotCorrAgainstRevenueGmv <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], gmv = data_in$gmv)
  p <- ggplot(data, aes(x = x, y = gmv)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$gmv, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}

################################################################################################################################################

## Filter data for July 2015 to June 2016.

consumerElectronicsData$Month <- as.numeric(consumerElectronicsData$Month)
consumerElectronicsData$Year <- as.numeric(consumerElectronicsData$Year)
consumerElectronicsDataForAnalysis <- consumerElectronicsData %>% filter((Year == 2015 & Month >= 7) | (Year == 2016 & Month <= 6))

consumerElectronicsDataForAnalysis$offer_price = consumerElectronicsDataForAnalysis$product_mrp*consumerElectronicsDataForAnalysis$units - consumerElectronicsDataForAnalysis$gmv

## Handling of NA's
NA.proportion <- function(x) mean(is.na(x))
table(NA.proportion=round(sapply(consumerElectronicsDataForAnalysis, NA.proportion), 2))

colSums(is.na(consumerElectronicsDataForAnalysis))
colMeans(is.na(consumerElectronicsDataForAnalysis))
barplot(colMeans(is.na(consumerElectronicsDataForAnalysis)))

## gmv, pincode and custid has null values
#DA: consumerElectronicsDataForAnalysis$gmv is NA for .29%. These records should be excluded from analysis.
#same records have cust_id as blank
consumerElectronicsDataForAnalysis <- consumerElectronicsDataForAnalysis %>% filter(!is.na(gmv))

## Lets confirm NA's again
colSums(is.na(consumerElectronicsDataForAnalysis))
colMeans(is.na(consumerElectronicsDataForAnalysis))
barplot(colMeans(is.na(consumerElectronicsDataForAnalysis)))

## No more NA's that we need to worry

##TODO: check near zero variance after aggregation

## Remove near zero variance variables which doesnt makese sense (For example, col having only one value is of no use)
nearZeroVariances <- nearZeroVar(consumerElectronicsDataForAnalysis, saveMetrics = TRUE)
nearZeroVariances_trues_indexes <- which(nearZeroVariances$nzv == TRUE)

## Units, deliverybdays, deliverycdays and product_analytic_super_category are columns that are near zero variance.

if (length(nearZeroVariances_trues_indexes) > 0) {
  consumerElectronicsDataForAnalysis <- consumerElectronicsDataForAnalysis[, -(nearZeroVariances_trues_indexes)]
}

## Based on above operation, columns <TODO: Put column names here> are removed becase these columns contains single value
################################################################################################################################################

consumerElectronicsDataForAnalysis$order_date <- as.Date(consumerElectronicsDataForAnalysis$order_date)
consumerElectronicsDataForAnalysis$week <- as.numeric(format(consumerElectronicsDataForAnalysis$order_date,"%W"))
consumerElectronicsDataForAnalysis$day <- as.numeric(format(consumerElectronicsDataForAnalysis$order_date,"%d"))

consumerElectronicsDataForAnalysis <- merge(consumerElectronicsDataForAnalysis, salesEventsWeeklyLevel, by = c("Year", "Month", "week"), all = TRUE)

consumerElectronicsDataForAnalysisForAggregation <- subset(consumerElectronicsDataForAnalysis, select = -c(X.U.FEFF.fsn_id, order_date)) %>% filter(product_analytic_sub_category == "HomeAudio" )

## Remove near zero variance variables which doesnt makese sense (For example, col having only one value is of no use)
nearZeroVariances1 <- nearZeroVar(consumerElectronicsDataForAnalysisForAggregation, saveMetrics = TRUE)
nearZeroVariances_trues_indexes1 <- which(nearZeroVariances1$nzv == TRUE)

## Units, deliverybdays, deliverycdays and product_analytic_super_category are columns that are near zero variance.

if (length(nearZeroVariances_trues_indexes1) > 0) {
  consumerElectronicsDataForAnalysisForAggregation <- consumerElectronicsDataForAnalysisForAggregation[, -(nearZeroVariances_trues_indexes1)]
}

dmyForAggregation <- dummyVars(" ~ .", data = consumerElectronicsDataForAnalysisForAggregation, fullRank=T)
consumerElectronicsDataForAnalysisForAggregationWithDummayVariables <- data.frame(predict(dmyForAggregation, newdata = consumerElectronicsDataForAnalysisForAggregation))

dayAggregationSplit1 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(Year, Month, day, gmv, product_mrp, offer_price) %>% group_by(Year, Month, day) %>% summarise_all(funs(sum), na.rm = TRUE)
dayAggregationSplit2 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(-c(gmv, order_id, order_item_id, sla, cust_id, pincode, week, deliverybdays, deliverycdays, product_procurement_sla)) %>% group_by(Year, Month, day) %>% summarise_all(funs(sum = sum), na.rm = TRUE)
dayAggregationSplit3 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(Year, Month, day, deliverybdays, deliverycdays, product_procurement_sla) %>% group_by(Year, Month, day) %>% summarise_all(funs(mean = mean), na.rm = TRUE)
dayAggregationSplit4 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(Year, Month, day, week) %>% group_by(Year, Month, day) %>% summarise(week = head(week, 1))
dayAggregationSplit5 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(Year, Month, day) %>% group_by(Year, Month, day) %>% summarise(totalHomeAccessoriesOrders = n())
dayAggregationSplit6 <- consumerElectronicsDataForAnalysis %>% dplyr::select(Year, Month, day) %>% group_by(Year, Month, day) %>% summarise(totalOrders = n())

consumerElectronicsDataForAnalysisDayAggregation <- Reduce(function(x, y) merge(x, y, by = c("Year", "Month", "day"), all=TRUE), list(dayAggregationSplit1, dayAggregationSplit2, dayAggregationSplit3, dayAggregationSplit4, dayAggregationSplit5, dayAggregationSplit6))

consumerElectronicsDataForAnalysisDayAggregation$homeAudioPropertionate <- consumerElectronicsDataForAnalysisDayAggregation$totalHomeAccessoriesOrders / consumerElectronicsDataForAnalysisDayAggregation$totalOrders

consumerElectronicsDataForAnalysisDayAggregation <- consumerElectronicsDataForAnalysisDayAggregation %>% filter(totalHomeAccessoriesOrders > 0)

daysInMonth <- consumerElectronicsDataForAnalysisDayAggregation %>% group_by(Year, Month) %>% summarise(days=n())
budgetByMonths <- merge(daysInMonth, budgetAllocations, by= c("Year", "Month"))


computeInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Total.Investment
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeTVInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$TV
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeDigitalInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Digital
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeSponsorshipInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Sponsorship
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeContentMarketingInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Content.Marketing
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( (investment/days) * 10000000 )
}

computeOnlinemarketingInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Online.marketing
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeAffiliatesInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Affiliates
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeSEMInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$SEM
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeRadioInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Radio
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

computeOtherInvestment <- function(record) {
  year <- as.numeric(record['Year'])
  month <- as.numeric(record['Month'])
  proportionate <- as.double(record['homeAudioPropertionate'])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Other
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return( proportionate * (investment/days) * 10000000 )
}

consumerElectronicsDataForAnalysisDayAggregation$investment <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentTV <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeTVInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentDigital <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeDigitalInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentSponsorship <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeSponsorshipInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentContentMarketing <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeContentMarketingInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentOnlinemarketing <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeOnlinemarketingInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentAffiliates <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeAffiliatesInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentSEM <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeSEMInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentRadio <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeRadioInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investmentOther <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeOtherInvestment)

################################################################################################################################################

## Side by side analysis of investment and revenue
## TODO: Updated variable names to have meaning full ones

weeklyRevenueVsInvestment <- consumerElectronicsDataForAnalysisDayAggregation %>% group_by(Year, week) %>% summarise(revenue = sum(gmv, na.rm=TRUE), investment = sum(investment, na.rm=TRUE))
temp <- as.data.frame((weeklyRevenueVsInvestment %>% filter(Year == 2015))[c(2,3,4)])
melted <- melt(temp, id.vars='week')

ggplot(melted, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

temp2 <- as.data.frame((weeklyRevenueVsInvestment %>% filter(Year == 2016))[c(2,3,4)])
melted2 <- melt(temp2, id.vars='week')

ggplot(melted2, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

################################################################################################################################################

## Weekly aggregation

weekAggregationSplit1 <- consumerElectronicsDataForAnalysisDayAggregation %>% dplyr::select(Year, Month, week, gmv, offer_price, investmentTV, investmentDigital, investmentSponsorship, investmentContentMarketing, investmentOnlinemarketing, investmentAffiliates, investmentSEM, investmentRadio, investmentOther) %>% group_by(Year, Month, week) %>% summarise_all(funs(sum), na.rm = TRUE)
weekAggregationSplit2 <- consumerElectronicsDataForAnalysisDayAggregation %>% dplyr::select(-c(deliverycdays_mean, deliverybdays_mean, product_procurement_sla_mean, homeAudioPropertionate, gmv, offer_price, day, investment, investmentTV, investmentDigital, investmentSponsorship, investmentContentMarketing, investmentOnlinemarketing, investmentAffiliates, investmentSEM, investmentRadio, investmentOther)) %>% group_by(Year, Month, week) %>% summarise_all(funs(sum = sum), na.rm = TRUE)
weekAggregationSplit3 <- consumerElectronicsDataForAnalysisDayAggregation %>% dplyr::select(Year, Month, week, deliverycdays_mean, deliverybdays_mean, product_procurement_sla_mean, homeAudioPropertionate) %>% group_by(Year, Month, week) %>% summarise_all(funs(mean), na.rm = TRUE)

#View(weekAggregationSplit1)
#View(weekAggregationSplit2)
#View(weekAggregationSplit3)
consumerElectronicsDataForAnalysisWeeklyAggregation <- Reduce(function(x, y) merge(x, y, by = c("Year", "Month", "week"), all=TRUE), list(weekAggregationSplit1, weekAggregationSplit2))

#consumerElectronicsDataForAnalysisWeeklyAggregation <- Reduce(function(x, y) merge(x, y, by = c("Year", "Month", "week"), all=TRUE), list(weekAggregationSplit1, weekAggregationSplit2, weekAggregationSplit3))

consumerElectronicsDataForAnalysisWeeklyAggregation <- merge(consumerElectronicsDataForAnalysisWeeklyAggregation, npsWeeklyLevel, by = c("Year", "Month", "week"), all = TRUE)
nrow(consumerElectronicsDataForAnalysisWeeklyAggregation)
consumerElectronicsDataForAnalysisWeeklyAggregation <- consumerElectronicsDataForAnalysisWeeklyAggregation %>% filter(!is.na(gmv))
nrow(consumerElectronicsDataForAnalysisWeeklyAggregation)

################################################################################################################################################

correlationMatrixWeekly <- cor(consumerElectronicsDataForAnalysisWeeklyAggregation, use = "pairwise.complete.obs")
#corrplot(correlationMatrixWeekly, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## Plot scatter plot for variables that have high correlation.
highcorrWeekly <- c(names(correlationMatrixWeekly[,'gmv'])[which(correlationMatrixWeekly[,'gmv'] > 0.8)], names(correlationMatrixWeekly[,'gmv'])[which(correlationMatrixWeekly[,'gmv'] < -0.8)])
data_corr_weekly <- consumerElectronicsDataForAnalysisWeeklyAggregation[,highcorrWeekly]
doPlots(data_corr_weekly, fun = plotCorrAgainstRevenueGmv, ii = 1:ncol(data_corr_weekly))

##---------------------------------------------------
## lets check only for investment against gmv
correlationMatrixWeeklyOnlyForInvestments <- cor(consumerElectronicsDataForAnalysisWeeklyAggregation %>% dplyr::select(union(starts_with("Investment"), starts_with("gmv"))), use = "pairwise.complete.obs")
corrplot(correlationMatrixWeeklyOnlyForInvestments, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## Plot scatter plot for variables that have high correlation.
highcorrWeeklyOnlyForInvestments <- c(names(correlationMatrixWeeklyOnlyForInvestments[,'gmv'])[which(correlationMatrixWeeklyOnlyForInvestments[,'gmv'] > 0.3)], names(correlationMatrixWeeklyOnlyForInvestments[,'gmv'])[which(correlationMatrixWeeklyOnlyForInvestments[,'gmv'] < -0.3)])
data_corr_weekly_only_for_investments <- consumerElectronicsDataForAnalysisWeeklyAggregation[,highcorrWeeklyOnlyForInvestments]
doPlots(data_corr_weekly_only_for_investments, fun = plotCorrAgainstRevenueGmv, ii = 1:ncol(data_corr_weekly_only_for_investments))
##---------------------------------------------------

################################################################################################################################################

##---------------- Start: Linear Regression model -------------##

dataset_final_analysis <- consumerElectronicsDataForAnalysisWeeklyAggregation
colnames(dataset_final_analysis)
View(dataset_final_analysis)
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(dataset_final_analysis), 0.7*nrow(dataset_final_analysis))
train = dataset_final_analysis[trainindices,]
test = dataset_final_analysis[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(gmv~.,data=train)
summary(model_1)

#Residual standard error: 39830 on 6 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:      1,	Adjusted R-squared:  0.9999 
#F-statistic:  8956 on 28 and 6 DF,  p-value: 7.668e-12

step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = gmv ~ Year + Month + week + offer_price + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentRadio + 
                investmentOther + product_mrp_sum + s1_fact.order_payment_typePrepaid_sum_sum + 
                product_analytic_verticalDock_sum_sum + product_analytic_verticalDockingStation_sum_sum + 
                product_analytic_verticalFMRadio_sum_sum + product_analytic_verticalHiFiSystem_sum_sum + 
                product_analytic_verticalKaraokePlayer_sum_sum + product_analytic_verticalSlingBox_sum_sum + 
                product_analytic_verticalVoiceRecorder_sum_sum + totalHomeAccessoriesOrders_sum + 
                totalOrders_sum + NPS_WeekAvg, data = train)

#let's check the summary of the model for R-squared and Adjusted R-squared
summary(model_2)
#Residual standard error: 31730 on 10 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:      1,	Adjusted R-squared:  0.9999 
#F-statistic: 1.646e+04 on 24 and 10 DF,  p-value: < 2.2e-16

#let's check for Multicollinearity
vif(model_2)

#remove variables having VIF > 2 and P > 0.005
#variables fits the condition and so should be removed
#Month,	week, s1_fact.order_payment_typePrepaid_sum_sum
#product_analytic_verticalDockingStation_sum_sum
#product_analytic_verticalHiFiSystem_sum_sum
#product_analytic_verticalSlingBox_sum_sum


#-------------------------------------------------
model_3 <- lm(formula = gmv ~ Year + offer_price + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentRadio + 
                investmentOther + product_mrp_sum +  
                product_analytic_verticalDock_sum_sum +  
                product_analytic_verticalFMRadio_sum_sum +  
                product_analytic_verticalKaraokePlayer_sum_sum +  
                product_analytic_verticalVoiceRecorder_sum_sum + totalHomeAccessoriesOrders_sum + 
                totalOrders_sum + NPS_WeekAvg, data = train)

#let's check the summary of the model for R-squared and Adjusted R-squared
summary(model_3)
#Residual standard error: 49410 on 16 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9998 
#F-statistic:  9052 on 18 and 16 DF,  p-value: < 2.2e-16

#let's check for Multicollinearity
vif(model_3)

#remove variables having VIF > 2 and P > 0.005
#variables fits the condition and so should be removed
#Year, investmentSponsorship, product_analytic_verticalDock_sum_sum
#product_analytic_verticalVoiceRecorder_sum_sum
#totalOrders_sum	NPS_WeekAvg

#-------------------------------------------------
model_4 <- lm(formula = gmv ~ offer_price + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentRadio + 
                investmentOther + product_mrp_sum +  
                product_analytic_verticalFMRadio_sum_sum +  
                product_analytic_verticalKaraokePlayer_sum_sum +  
                totalHomeAccessoriesOrders_sum, data = train)

#let's check the summary of the model for R-squared and Adjusted R-squared
summary(model_4)
#Residual standard error: 45310 on 28 degrees of freedom
#Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9998 
#F-statistic: 1.847e+04 on 12 and 28 DF,  p-value: < 2.2e-16

#let's check for Multicollinearity
vif(model_4)

#remove variables having VIF > 2 and P > 0.005
#variables fits the condition and so should be removed
#NO VARIABLES TO DELETE


# ----- let's test  the model ------

# predicting the results in test dataset
Predict_1 <- predict(model_4,test)
test$test_gmv <- Predict_1
View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$gmv,test$test_gmv)
rsquared <- cor(test$gmv,test$test_gmv)^2
rsquared
#0.9993642

# R Squared value of the test data is 0.9993642 which is really good so model could be recommended to be accepted

##---------------- End: Linear Regression model -------------##
