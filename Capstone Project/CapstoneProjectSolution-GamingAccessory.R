## Instruction to reviewer: Please make sure to set current working directory to same directory
## where all the files are present

## IIITB - Group_Facilitator_RollNo: DDA1730041
## Team:
## 1) Fayiz Mayam Veettil
## 2) Merin Jose
## 3) Deepak Aneja
## 4) Suresh Balla
################################################################################################################################################

## Business Objective
## TODO: Place verbiage from assignment page

## Begin of Install and load required libraries

load.libraries <- c('reshape', 'DataCombine', 'stringr', 'dplyr', 'data.table', 'e1071', 'gridExtra', 'corrplot', 'ggplot2', 'tidyr', 'MASS', 'car', 'caret', 'GGally', 'mice','cowplot','caTools')

install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

## End of Install and load required libraries
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


## Load data sets
#DA Handed the \N for Null values
consumerElectronicsData <- read.csv("ConsumerElectronics.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("\\N", "NA","NaN","","#DIV/0!"))
budgetAllocationsWeekly <- read.csv("budget_allocation_weekly.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))

#DA Load Sales Event and NPS data
salesEventsWeeklyLevel <- read.csv("events_salesdays.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
npsWeeklyLevel <- read.csv("NPS.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))

colnames(budgetAllocationsWeekly)[1] <- "Year"
colnames(salesEventsWeeklyLevel)[1] <- "Year"

nrow(consumerElectronicsData)
nrow(distinct(consumerElectronicsData))

consumerElectronicsData <- distinct(consumerElectronicsData)

## End of load data sets
################################################################################################################################################

## Filter data for July 2015 to June 2016.

consumerElectronicsData$Month <- as.numeric(consumerElectronicsData$Month)
consumerElectronicsData$Year <- as.numeric(consumerElectronicsData$Year)
consumerElectronicsDataForAnalysis <- consumerElectronicsData %>% filter((Year == 2015 & Month >= 7) | (Year == 2016 & Month <= 6))

consumerElectronicsDataForAnalysis$offer_percentage = (consumerElectronicsDataForAnalysis$product_mrp*consumerElectronicsDataForAnalysis$units - consumerElectronicsDataForAnalysis$gmv)/consumerElectronicsDataForAnalysis$product_mrp*consumerElectronicsDataForAnalysis$units

## Lets calculate if a product is premium or not based on its MRP in its own category

products_quantiles <- consumerElectronicsDataForAnalysis %>% group_by(product_analytic_category, product_analytic_sub_category, product_analytic_vertical) %>% summarise(`75%`=quantile(product_mrp, probs=0.75), `25%`=quantile(product_mrp, probs=0.25))

consumerElectronicsDataForAnalysis <- merge(consumerElectronicsDataForAnalysis, products_quantiles, all.x = TRUE)
consumerElectronicsDataForAnalysis$product_mrp_class <- ifelse(consumerElectronicsDataForAnalysis$product_mrp >= consumerElectronicsDataForAnalysis$`75%`, "premium", ifelse(consumerElectronicsDataForAnalysis$product_mrp < consumerElectronicsDataForAnalysis$`25%`, "cheap", "medium"))

colnames(consumerElectronicsDataForAnalysis)

consumerElectronicsDataForAnalysis <- subset(consumerElectronicsDataForAnalysis, select = -c(`75%`, `25%`))


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

consumerElectronicsDataForAnalysis <- distinct(consumerElectronicsDataForAnalysis)
consumerElectronicsDataForAnalysis <- merge(consumerElectronicsDataForAnalysis, salesEventsWeeklyLevel, by = c("Year", "Month", "week"), all.x = TRUE)

## Check for records with orders with revenue per unit more than MRP, negative SLA or product_procurement_sla. Cap the GMV accordingly so we do not loose revenue as we budget for all days in a month
nrow(consumerElectronicsDataForAnalysis %>% filter(offer_percentage < 0 | sla < 0 | product_procurement_sla < 0))
consumerElectronicsDataForAnalysis$gmv = ifelse(consumerElectronicsDataForAnalysis$offer_percentage < 0, consumerElectronicsDataForAnalysis$product_mrp*consumerElectronicsDataForAnalysis$units, consumerElectronicsDataForAnalysis$gmv)
consumerElectronicsDataForAnalysis$sla = ifelse(consumerElectronicsDataForAnalysis$sla < 0, NULL, consumerElectronicsDataForAnalysis$sla)
consumerElectronicsDataForAnalysis$product_procurement_sla = ifelse(consumerElectronicsDataForAnalysis$product_procurement_sla < 0, 0, consumerElectronicsDataForAnalysis$product_procurement_sla)
consumerElectronicsDataForAnalysis$offer_percentage = ifelse(consumerElectronicsDataForAnalysis$offer_percentage < 0, 0, consumerElectronicsDataForAnalysis$offer_percentage)
nrow(consumerElectronicsDataForAnalysis %>% filter(offer_percentage < 0 | sla < 0 | product_procurement_sla < 0))

consumerElectronicsDataForAnalysisForAggregation <- subset(consumerElectronicsDataForAnalysis, select = -c(X.U.FEFF.fsn_id, order_date)) %>% filter(product_analytic_sub_category == "GamingAccessory" )

categorical_variables_indexes <- as.integer(which(sapply(consumerElectronicsDataForAnalysisForAggregation, is.character)))

weekAggregationSplit1 <- consumerElectronicsDataForAnalysisForAggregation %>% dplyr::select(Year, week, gmv, product_mrp) %>% group_by(Year, week) %>% summarise_all(funs(sum), na.rm = TRUE)
weekAggregationSplit2 <- consumerElectronicsDataForAnalysisForAggregation %>% dplyr::select(Year, week, deliverycdays, deliverybdays, product_procurement_sla, offer_percentage) %>% group_by(Year, week) %>% summarise_all(funs(mean), na.rm = TRUE)

consumerElectronicsDataForAnalysisWeeklyAggregation <- merge(weekAggregationSplit1, weekAggregationSplit2, by = c("Year", "week"), all.x=TRUE)

for (category_variable_index in categorical_variables_indexes) {
  dataFrameTemp <- consumerElectronicsDataForAnalysisForAggregation %>% 
    dplyr::select(Year, week, !!colnames(consumerElectronicsDataForAnalysisForAggregation)[category_variable_index]) %>% 
    group_by_at(c("Year", "week", colnames(consumerElectronicsDataForAnalysisForAggregation)[category_variable_index])) %>%
    summarise(count = n()) %>% 
    mutate(prop = count / sum(count) )
  
  dataFrameTemp[[3]] <- paste0(colnames(consumerElectronicsDataForAnalysisForAggregation)[category_variable_index], "_", dataFrameTemp[[3]])
  temp <- dcast(dataFrameTemp, paste0("Year + week ~ ", colnames(consumerElectronicsDataForAnalysisForAggregation)[category_variable_index]), value.var="prop")
  consumerElectronicsDataForAnalysisWeeklyAggregation <- merge(consumerElectronicsDataForAnalysisWeeklyAggregation, temp, by = c("Year", "week"), all.x=TRUE)
  remove(dataFrameTemp)
  remove(temp)
}

consumerElectronicsDataForAnalysisWeeklyAggregation <- merge(consumerElectronicsDataForAnalysisWeeklyAggregation, npsWeeklyLevel, by = c("Year", "week"), all = TRUE)
consumerElectronicsDataForAnalysisWeeklyAggregation <- merge(consumerElectronicsDataForAnalysisWeeklyAggregation, budgetAllocationsWeekly, by = c("Year", "week"), all = TRUE)

nrow(consumerElectronicsDataForAnalysisWeeklyAggregation)

colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Total.Investment'] <- 'investment'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'TV'] <- 'investmentTV'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Digital'] <- 'investmentDigital'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Sponsorship'] <- 'investmentSponsorship'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Content.Marketing'] <- 'investmentContentMarketing'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Online.marketing'] <- 'investmentOnlinemarketing'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Affiliates'] <- 'investmentAffiliates'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'SEM'] <- 'investmentSEM'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Radio'] <- 'investmentRadio'
colnames(consumerElectronicsDataForAnalysisWeeklyAggregation)[colnames(consumerElectronicsDataForAnalysisWeeklyAggregation) == 'Other'] <- 'investmentOther'

################################################################################################################################################

## Side by side analysis of investment and revenue
## TODO: Updated variable names to have meaning full ones

weeklyRevenueVsInvestment <- consumerElectronicsDataForAnalysisWeeklyAggregation %>% dplyr::select(Year, week, gmv, investment)
temp <- as.data.frame((weeklyRevenueVsInvestment %>% filter(Year == 2015))[c(2,3,4)])
melted <- melt(temp, id.vars='week')

ggplot(melted, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

temp2 <- as.data.frame((weeklyRevenueVsInvestment %>% filter(Year == 2016))[c(2,3,4)])
melted2 <- melt(temp2, id.vars='week')

ggplot(melted2, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

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

dataset_final_analysis <- consumerElectronicsDataForAnalysisWeeklyAggregation
#colnames(dataset_final_analysis)
#View(dataset_final_analysis)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataset_final_analysis <- dataset_final_analysis[-c(9:26)]
dataset_final_analysis <- dataset_final_analysis[-c(4)]


##########Linear Model........

linear_data_set <- dataset_final_analysis
linear_data_set[is.nan(linear_data_set)] <- 0
linear_data_set[is.na(linear_data_set)] <- 0
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(linear_data_set), 0.8*nrow(linear_data_set))
train = linear_data_set[trainindices,]
test = linear_data_set[-trainindices,]

model_1 <-lm(gmv ~ .,data=train)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step

model_2 <-   lm(formula = gmv ~ week + deliverybdays + product_procurement_sla + 
                product_mrp_class_cheap + Event_BED + `Event_Christmas & New Year` + 
                Event_Dussehra + Event_FHSD + Event_Independence + Event_NA + 
                investment + investmentTV + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentSEM + investmentRadio + 
                NPS_WeekAvg, data = train)
summary(model_2)


vif(model_2)

#NPS_WeekAvg,Event_Dussehra,Event_Independence

model_3 <- lm(formula = gmv ~ week + deliverybdays + product_procurement_sla + 
                product_mrp_class_cheap + Event_BED + `Event_Christmas & New Year` + 
                Event_FHSD + Event_NA + 
                investment + investmentTV + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentSEM + investmentRadio
                , data = train)
summary(model_3)


vif(model_3)
#Event_FHSD

model_4 <-   lm(formula = gmv ~ week + deliverybdays + product_procurement_sla + 
                product_mrp_class_cheap + Event_BED + `Event_Christmas & New Year` + 
                Event_NA + 
                investment + investmentTV + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentSEM + investmentRadio
              , data = train)
summary(model_4)

#investment, product_mrp_class_cheap

model_5 <- lm(formula = gmv ~ week + deliverybdays + product_procurement_sla + 
                product_mrp_class_cheap + Event_BED + `Event_Christmas & New Year` + 
                Event_NA + 
                investment + investmentTV + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentSEM + investmentRadio
              , data = train)
summary(model_5)


vif(model_5)

# predicting the results in test dataset
Predict_1 <- predict(model_5,test)
test$test_gmv <- Predict_1
#View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$gmv,test$test_gmv)
rsquared <- cor(test$gmv,test$test_gmv)^2
rsquared
# 

#######multiplicative model

log_data_set <- dataset_final_analysis
log_data_set[is.nan(log_data_set)] <- 0.01
log_data_set[is.na(log_data_set)] <- 0.01
log_data_set <- log(log_data_set)
log_data_set[mapply(is.infinite, log_data_set)] <- 1

model_1 <- lm(gmv~.,log_data_set)

summary(model_1)

step <- stepAIC(model_1, direction="both")
step


model_2 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BED + 
                Event_BSD + `Event_Christmas & New Year` + Event_Diwali + 
                Event_Dussehra + `Event_Eid & Rathayatra` + Event_FHSD + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                Event_Republic + Event_Vday + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_2)
vif(model_2)


#Event_Vday

model_3 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BED + 
                Event_BSD + `Event_Christmas & New Year` + Event_Diwali + 
                Event_Dussehra + `Event_Eid & Rathayatra` + Event_FHSD + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                Event_Republic + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_3)
vif(model_3)
#`Event_Christmas & New Year`

model_4 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BED + 
                Event_BSD + Event_Diwali + 
                Event_Dussehra + `Event_Eid & Rathayatra` + Event_FHSD + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                Event_Republic + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_4)
vif(model_4)

# Event_BED,`Event_Eid & Rathayatra`

model_4 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_BSD + Event_Diwali + 
                Event_Dussehra  + Event_FHSD + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                Event_Republic + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_4)

#Event_Republic

model_5 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_BSD + Event_Diwali + 
                Event_Dussehra  + Event_FHSD + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_5)
 
#Event_Republic

model_5 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_BSD + Event_Diwali + 
                Event_Dussehra  + Event_FHSD + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_5)
#Event_BSD
model_6 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Diwali + 
                Event_Dussehra  + Event_FHSD + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_6)

#Event_FHSD

model_7 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Diwali + 
                Event_Dussehra + 
                Event_Independence + Event_NA + Event_Pacman + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_7)

#Event_NA

model_8 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Diwali + 
                Event_Dussehra + 
                Event_Independence + Event_Pacman + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_8)

#Event_Pacman, Event_Dussehra
model_9 <- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Diwali + 
                Event_Independence  + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_9)

#Event_Diwali, investmentAffiliates
model_10<- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Independence  + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentOnlinemarketing + investmentSEM + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_10)

#investmentSEM, investmentDigital, investmentSponsorship
model_10<- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Independence  + Event_Rakshabandhan + 
                NPS_WeekAvg + investmentTV + 
                investmentContentMarketing + 
                investmentOnlinemarketing + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_10)
#NPS_WeekAvg, 

model_11<- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Independence  + Event_Rakshabandhan + 
                investmentTV + 
                investmentOnlinemarketing + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_11)

#Event_Independence, product_mrp_class_medium, Event_Rakshabandhan
model_12<- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_premium + 
                investmentTV + 
                investmentOnlinemarketing + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_12)

#product_mrp_class_premium

model_13<- lm(formula = gmv ~ Year + offer_percentage + s1_fact.order_payment_type_COD + 
                investmentTV + 
                investmentOnlinemarketing + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_13)

#offer_percentage
model_14<- lm(formula = gmv ~ Year + s1_fact.order_payment_type_COD + 
                investmentTV + 
                investmentOnlinemarketing + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_14)



#Year
model_15<- lm(formula = gmv ~ s1_fact.order_payment_type_COD + 
                investmentTV + 
                investmentOnlinemarketing + 
                investmentRadio + investmentOther, data = log_data_set)

summary(model_15)


Predict_1 <- predict(model_15,log_data_set)
log_data_set$test_gmv <- Predict_1
#View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(log_data_set$gmv,log_data_set$test_gmv)
rsquared <- cor(log_data_set$gmv,log_data_set$test_gmv)^2
rsquared

############################Distributed lag models

Dis_Model <- dataset_final_analysis

Dis_model_1 <- slide(Dis_Model, Var = "gmv",slideBy = -1)
Dis_model_1 <- Dis_model_1[-c(3)]

#Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -2)

#Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -3)

Dis_model_1[is.nan(Dis_model_1)] <- 0
Dis_model_1[is.na(Dis_model_1)] <- 0

#Dis_model <- scale(Dis_model)
#Dis_model <- data.frame(Dis_model)

dist_model <- lm(`gmv-1` ~.,Dis_model_1)

summary(dist_model)


model_2 <- stepAIC(dist_model,direction = "both")

summary(model_2)

vif(model_2)


#investmentOther
model_3 <- lm(formula = `gmv-1` ~ week + deliverycdays + deliverybdays + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investment + investmentTV + 
                investmentDigital + investmentSponsorship + investmentContentMarketing + 
                investmentAffiliates + investmentRadio + investmentOther, 
                data = Dis_model_1)


summary(model_3)

vif(model_3)
#investmentSponsorship
model_4 <- lm(formula = `gmv-1` ~ week + deliverycdays + deliverybdays + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investment + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentAffiliates + investmentRadio + investmentOther, 
                data = Dis_model_1)


summary(model_4)

vif(model_4)
#deliverybdays

model_5 <- lm(formula = `gmv-1` ~ week + deliverycdays + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investment + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentAffiliates + investmentRadio + investmentOther, 
                data = Dis_model_1)


summary(model_5)

vif(model_5)

#deliverycdays

model_6 <- lm(formula = `gmv-1` ~ week + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investment + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentAffiliates + investmentRadio + investmentOther, 
                data = Dis_model_1)


summary(model_6)

vif(model_6)

#investment


model_7 <- lm(formula = `gmv-1` ~ week + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentAffiliates + investmentRadio + investmentOther, 
                data = Dis_model_1)


summary(model_7)

vif(model_7)

#Event_BSD

model_8 <- lm(formula = `gmv-1` ~ week + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentAffiliates + investmentRadio + investmentOther, 
                data = Dis_model_1)
summary(model_8)

#investmentAffiliates

model_9 <- lm(formula = `gmv-1` ~ week + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentRadio + investmentOther, 
                data = Dis_model_1)
summary(model_9)

#offer_percentage'

model_9 <- lm(formula = `gmv-1` ~ week + 
                product_procurement_sla + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_NA + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentRadio + investmentOther, 
              data = Dis_model_1)
summary(model_9)
#Event_NA
model_10 <- lm(formula = `gmv-1` ~ week + 
                product_procurement_sla + s1_fact.order_payment_type_COD + 
                product_mrp_class_cheap + product_mrp_class_medium + 
                `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Independence + Event_Pacman + 
                Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                investmentDigital + investmentContentMarketing + 
                investmentRadio + investmentOther, 
              data = Dis_model_1)
summary(model_10)

#Event_Independence

model_11 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + s1_fact.order_payment_type_COD + 
                 product_mrp_class_cheap + product_mrp_class_medium + 
                 `Event_Christmas & New Year` + Event_Dussehra + `Event_Eid & Rathayatra` + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                 investmentDigital + investmentContentMarketing + 
                 investmentRadio + investmentOther, 
               data = Dis_model_1)
summary(model_11)

#`Event_Christmas & New Year`

model_12 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + s1_fact.order_payment_type_COD + 
                 product_mrp_class_cheap + product_mrp_class_medium + 
                 Event_Dussehra + `Event_Eid & Rathayatra` + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                 investmentDigital + investmentContentMarketing + 
                 investmentRadio + investmentOther, 
               data = Dis_model_1)
summary(model_12)
#s1_fact.order_payment_type_COD

model_13 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + 
                 product_mrp_class_cheap + product_mrp_class_medium + 
                 Event_Dussehra + `Event_Eid & Rathayatra` + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                 investmentDigital + investmentContentMarketing + 
                 investmentRadio + investmentOther, 
               data = Dis_model_1)
summary(model_13)
#investmentRadio
model_14 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + 
                 product_mrp_class_cheap + product_mrp_class_medium + 
                 Event_Dussehra + `Event_Eid & Rathayatra` + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + investmentTV + 
                 investmentDigital + investmentContentMarketing + 
                 investmentOther, 
               data = Dis_model_1)
summary(model_14)


#investmentTV
model_14 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + 
                 product_mrp_class_cheap + product_mrp_class_medium + 
                 Event_Dussehra + `Event_Eid & Rathayatra` + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + 
                 investmentDigital + investmentContentMarketing + 
                 investmentOther, 
                 data = Dis_model_1)
summary(model_14)
#investmentOther
model_15 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + 
                 product_mrp_class_cheap + product_mrp_class_medium + 
                 Event_Dussehra + `Event_Eid & Rathayatra` + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + 
                 investmentDigital + investmentContentMarketing
                 , 
               data = Dis_model_1)
summary(model_15)
#`Event_Eid & Rathayatra`

model_16 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + 
                 product_mrp_class_cheap + product_mrp_class_medium + 
                 Event_Dussehra + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + 
                 investmentDigital + investmentContentMarketing, 
               data = Dis_model_1)
summary(model_16)

#product_mrp_class_cheap
model_17 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + 
                 product_mrp_class_medium + 
                 Event_Dussehra + 
                 Event_FHSD + Event_Pacman + 
                 Event_Rakshabandhan + NPS_WeekAvg + 
                 investmentDigital + investmentContentMarketing, 
               data = Dis_model_1)
summary(model_17)
#Event_Pacman
model_18 <- lm(formula = `gmv-1` ~ week + 
                 product_procurement_sla + 
                 product_mrp_class_medium + 
                 Event_Dussehra + 
                 Event_FHSD + 
                 Event_Rakshabandhan + NPS_WeekAvg + 
                 investmentDigital + investmentContentMarketing, 
               data = Dis_model_1)
summary(model_18)

Predict_1 <- predict(model_18,Dis_model_1)
Dis_model_1$test_gmv <- Predict_1
#View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(Dis_model_1$gmv,Dis_model_1$test_gmv)
rsquared <- cor(Dis_model_1$gmv,Dis_model_1$test_gmv)^2
rsquared

###########Multiplicative + distributed model
mult_Dis_Model <- dataset_final_analysis

mult_Dis_model_1 <- slide(mult_Dis_Model, Var = "gmv",slideBy = -1)
colnames(mult_Dis_model_1)
mult_Dis_model_1 <- mult_Dis_model_1[-c(3)]

mult_Dis_model_1[is.nan(mult_Dis_model_1)] <- 0.01
mult_Dis_model_1[is.na(mult_Dis_model_1)] <- 0.01

mult_Dis_model_1 <- log(mult_Dis_model_1)
mult_Dis_model_1[mapply(is.infinite, mult_Dis_model_1)] <- 1

mult_dist_model <- lm(`gmv-1`~.,mult_Dis_model_1)

summary(mult_dist_model)


model_2 <- stepAIC(mult_dist_model,direction = "both")

summary(model_2)

vif(model_2)
#`Event_Christmas & New Year`,week

model_3 <- lm(formula = `gmv-1` ~ Year + week + deliverycdays + deliverybdays + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + product_mrp_class_cheap + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BED + 
                `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Rakshabandhan + Event_Republic + Event_Vday + 
                NPS_WeekAvg + investment + investmentTV + investmentDigital + 
                investmentContentMarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_3)

#offer_percentage 

model_4 <- lm(formula = `gmv-1` ~ Year + week + deliverycdays + deliverybdays + 
                product_procurement_sla + s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + product_mrp_class_cheap + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BED + 
                `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Rakshabandhan + Event_Republic + Event_Vday + 
                NPS_WeekAvg + investment + investmentTV + investmentDigital + 
                investmentContentMarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)
summary(model_4)

#product_procurement_sla

model_5 <- lm(formula = `gmv-1` ~ Year + week + deliverycdays + deliverybdays + 
                s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + product_mrp_class_cheap + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BED + 
                `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_Rakshabandhan + Event_Republic + Event_Vday + 
                NPS_WeekAvg + investment + investmentTV + investmentDigital + 
                investmentContentMarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_5)

#Event_FHSD


model_5 <- lm(formula = `gmv-1` ~ Year + week + deliverycdays + deliverybdays + 
                s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + product_mrp_class_cheap + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BED + 
                `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
                Event_Rakshabandhan + Event_Republic + Event_Vday + 
                NPS_WeekAvg + investment + investmentTV + investmentDigital + 
                investmentContentMarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_5)

#Event_Vday,Event_BED,deliverycdays,deliverybdays
model_6 <- lm(formula = `gmv-1` ~ Year + week +
                s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + product_mrp_class_cheap + 
                product_mrp_class_medium + product_mrp_class_premium + 
                `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
                Event_Rakshabandhan + Event_Republic + 
                NPS_WeekAvg + investment + investmentTV + investmentDigital + 
                investmentContentMarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_6)

#product_mrp_class_cheap, `Event_Christmas & New Year`, Event_Republic 

model_7 <- lm(formula = `gmv-1` ~ Year + week +
                s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid  + 
                product_mrp_class_medium + product_mrp_class_premium + 
                Event_Diwali + `Event_Eid & Rathayatra` + 
                Event_Rakshabandhan + 
                NPS_WeekAvg + investment + investmentTV + investmentDigital + 
                investmentContentMarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_7)

#Event_Rakshabandhan, Event_Diwali


model_8 <- lm(formula = `gmv-1` ~ Year + week +
                s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid  + 
                product_mrp_class_medium + product_mrp_class_premium + 
                `Event_Eid & Rathayatra` + 
                NPS_WeekAvg + investment + investmentTV + investmentDigital + 
                investmentContentMarketing + investmentAffiliates + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_8)

#order_payment_type_Prepaid, investmentTV,investmentAffiliates

model_9 <- lm(formula = `gmv-1` ~ Year + week +
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + 
                `Event_Eid & Rathayatra` + 
                NPS_WeekAvg + investment + investmentDigital + 
                investmentContentMarketing + investmentSEM + 
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_9)
##investmentContentMarketing,NPS_WeekAvg,`Event_Eid & Rathayatra`,
model_10 <- lm(formula = `gmv-1` ~ Year + week +
                s1_fact.order_payment_type_COD + 
                product_mrp_class_premium + 
                investmentDigital + product_mrp_class_medium +
                 investmentSEM + investment +
                investmentRadio + investmentOther, data = mult_Dis_model_1)

summary(model_10)


Predict_1 <- predict(model_10,mult_Dis_model_1)
mult_Dis_model_1$test_gmv <- Predict_1
#View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(mult_Dis_model_1$gmv,mult_Dis_model_1$test_gmv)
rsquared <- cor(mult_Dis_model_1$gmv,mult_Dis_model_1$test_gmv)^2
rsquared

