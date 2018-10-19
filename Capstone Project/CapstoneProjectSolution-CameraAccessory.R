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

load.libraries <- c('reshape', 'stringr', 'dplyr', 'data.table', 'e1071', 'gridExtra', 'corrplot', 'ggplot2', 'tidyr', 'MASS', 'car', 'caret', 'GGally', 'mice','cowplot','caTools')
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

consumerElectronicsDataForAnalysisForAggregation <- subset(consumerElectronicsDataForAnalysis, select = -c(X.U.FEFF.fsn_id, order_date)) %>% filter(product_analytic_sub_category == "CameraAccessory" )

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
##---------------------------------------------------

################################################################################################################################################

dataset_final_analysis <- consumerElectronicsDataForAnalysisWeeklyAggregation
colnames(dataset_final_analysis)
#View(dataset_final_analysis)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataset_final_analysis <- dataset_final_analysis[-c(9:34)]
dataset_final_analysis <- dataset_final_analysis[-c(4)]

########linear model.......

linear_data_set <- dataset_final_analysis
linear_data_set[is.nan(linear_data_set)] <- 0
linear_data_set[is.na(linear_data_set)] <- 0



# separate training and testing data

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(linear_data_set), 0.8*nrow(linear_data_set))
train = linear_data_set[trainindices,]
test = linear_data_set[-trainindices,]


model_1 <-lm(formula = gmv ~ .,data=train)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step


model_2 <-lm(formula = gmv ~ Year + week + deliverycdays + deliverybdays + 
               product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
               s1_fact.order_payment_type_Prepaid + Event_BED + Event_BSD + 
               `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
               Event_FHSD + Event_NA + Event_Pacman + investment + investmentTV + 
               investmentSponsorship + investmentContentMarketing + investmentOnlinemarketing + 
               investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_2)

vif(model_2)
#deliverycdays
model_3 <-lm(formula = gmv ~ Year + week + deliverybdays + 
               product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
               s1_fact.order_payment_type_Prepaid + Event_BED + Event_BSD + 
               `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
               Event_FHSD + Event_NA + Event_Pacman + investment + investmentTV + 
               investmentSponsorship + investmentContentMarketing + investmentOnlinemarketing + 
               investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_3)

vif(model_3)

#Event_Pacman

model_4 <-lm(formula = gmv ~ Year + week + deliverybdays + 
               product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
               s1_fact.order_payment_type_Prepaid + Event_BED + Event_BSD + 
               `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
               Event_FHSD + Event_NA + investment + investmentTV + 
               investmentSponsorship + investmentContentMarketing + investmentOnlinemarketing + 
               investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_4)

vif(model_4)
#Event_BSD

model_5 <-lm(formula = gmv ~ Year + week + deliverybdays + 
               product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
               s1_fact.order_payment_type_Prepaid + Event_BED  + 
               `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
               Event_FHSD + Event_NA + investment + investmentTV + 
               investmentSponsorship + investmentContentMarketing + investmentOnlinemarketing + 
               investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_5)

vif(model_5)
#deliverybdays
model_6 <-lm(formula = gmv ~ Year + week + 
               product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
               s1_fact.order_payment_type_Prepaid + Event_BED  + 
               `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
               Event_FHSD + Event_NA + investment + investmentTV + 
               investmentSponsorship + investmentContentMarketing + investmentOnlinemarketing + 
               investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_6)

vif(model_6)


#Year
model_7 <-lm(formula = gmv ~ week + 
               product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
               s1_fact.order_payment_type_Prepaid + Event_BED  + 
               `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
               Event_FHSD + Event_NA + investment + investmentTV + 
               investmentSponsorship + investmentContentMarketing + investmentOnlinemarketing + 
               investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_7)

vif(model_7)

#investmentContentMarketing
model_8 <- lm(formula = gmv ~ week + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + Event_BED  + 
                `Event_Christmas & New Year` + Event_Diwali + `Event_Eid & Rathayatra` + 
                Event_FHSD + Event_NA + investment + investmentTV + 
                investmentSponsorship + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_8)

vif(model_8)
#`Event_Eid & Rathayatra`

model_9 <- lm(formula = gmv ~ week + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + Event_BED  + 
                `Event_Christmas & New Year` + Event_Diwali + 
                Event_FHSD + Event_NA + investment + investmentTV + 
                investmentSponsorship + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_9)

vif(model_9)


#Event_FHSD
model_10 <-lm(formula = gmv ~ week + 
                product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                s1_fact.order_payment_type_Prepaid + Event_BED  + 
                `Event_Christmas & New Year` + Event_Diwali + 
                Event_NA + investment + investmentTV + 
                investmentSponsorship + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio, data = train)
summary(model_10)

vif(model_10)

#investmentAffiliates

model_11 <- lm(formula = gmv ~ week + 
                 product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                 s1_fact.order_payment_type_Prepaid + Event_BED  + 
                 `Event_Christmas & New Year` + Event_Diwali + 
                 Event_NA + investment + investmentTV + 
                 investmentSponsorship + investmentOnlinemarketing + 
                 investmentSEM + investmentRadio, data = train)
summary(model_11)

vif(model_11)


#investmentOnlinemarketing

model_12 <- lm(formula = gmv ~ week + 
                 product_procurement_sla + offer_percentage + s1_fact.order_payment_type_COD + 
                 s1_fact.order_payment_type_Prepaid + Event_BED  + 
                 `Event_Christmas & New Year` + Event_Diwali + 
                 Event_NA + investment + investmentTV + 
                 investmentSponsorship + 
                 investmentSEM + investmentRadio, data = train)
summary(model_12)


Predict_1 <- predict(model_12,test)
test$test_gmv <- Predict_1
#View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$gmv,test$test_gmv)
rsquared <- cor(test$gmv,test$test_gmv)^2
rsquared



####################multiplicative model

log_data_set <- dataset_final_analysis
log_data_set[is.nan(log_data_set)] <- 0.01
log_data_set[is.na(log_data_set)] <- 0.01

log_data_set <- log(log_data_set)
log_data_set[mapply(is.infinite, log_data_set)] <- 1

model_1 <- lm(gmv~.,log_data_set)

summary(model_1)

step <- stepAIC(model_1, direction="both")
step


model_2 <- lm(formula = gmv ~ Year + deliverycdays + deliverybdays + offer_percentage + 
                s1_fact.order_payment_type_COD + s1_fact.order_payment_type_Prepaid + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + Event_Republic + Event_Vday + NPS_WeekAvg + 
                investment + investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_2)

vif(model_2)



#investment
model_3 <- lm(formula = gmv ~ Year + deliverycdays + deliverybdays + offer_percentage + 
                s1_fact.order_payment_type_COD + s1_fact.order_payment_type_Prepaid + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + Event_Republic + Event_Vday + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_3)

#deliverycdays

model_4 <- lm(formula = gmv ~ Year + deliverybdays + offer_percentage + 
                s1_fact.order_payment_type_COD + s1_fact.order_payment_type_Prepaid + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + Event_Republic + Event_Vday + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_4)

#Event_Vday
model_5 <- lm(formula = gmv ~ Year + deliverybdays + offer_percentage + 
                s1_fact.order_payment_type_COD + s1_fact.order_payment_type_Prepaid + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + Event_Republic + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_5)

#s1_fact.order_payment_type_Prepaid


model_6 <- lm(formula = gmv ~ Year + deliverybdays + offer_percentage + 
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + Event_Republic + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_6)


#deliverybdays

model_7 <- lm(formula = gmv ~ Year + offer_percentage + 
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                `Event_Christmas & New Year` + Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + Event_Republic + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_7)
#`Event_Christmas & New Year`

model_8 <- lm(formula = gmv ~ Year + offer_percentage + 
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + Event_Republic + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_8)

#Event_Republic

model_9 <- lm(formula = gmv ~ Year + offer_percentage + 
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_9)

#offer_percentage

model_9 <- lm(formula = gmv ~ Year +  
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_NA + Event_Pacman + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_9)

#Event_NA
model_9 <- lm(formula = gmv ~ Year +  
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_BSD + 
                Event_Diwali + Event_Dussehra + 
                `Event_Eid & Rathayatra` + Event_FHSD + Event_Independence + 
                Event_Pacman + NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_9)
#Event_BSD + Event_Diwali + Event_Dussehra +  `Event_Eid & Rathayatra` + Event_FHSD+Event_Pacman

model_11 <- lm(formula = gmv ~ Year +  
                s1_fact.order_payment_type_COD + 
                product_mrp_class_medium + product_mrp_class_premium + Event_Independence + 
                NPS_WeekAvg + 
                investmentTV + investmentDigital + investmentSponsorship + 
                investmentContentMarketing + investmentOnlinemarketing + 
                investmentAffiliates + investmentSEM + investmentRadio + 
                investmentOther, data = log_data_set)

summary(model_11)


#NPS_WeekAvg

model_12 <- lm(formula = gmv ~ Year +  
                 s1_fact.order_payment_type_COD + 
                 product_mrp_class_medium + product_mrp_class_premium + Event_Independence + 
                 investmentTV + investmentDigital + investmentSponsorship + 
                 investmentContentMarketing + investmentOnlinemarketing + 
                 investmentAffiliates + investmentSEM + investmentRadio + 
                 investmentOther, data = log_data_set)

summary(model_12)
#investmentRadio

model_13 <- lm(formula = gmv ~ Year +  
                 s1_fact.order_payment_type_COD + 
                 product_mrp_class_medium + product_mrp_class_premium + Event_Independence + 
                 investmentTV + investmentDigital + investmentSponsorship + 
                 investmentContentMarketing + investmentOnlinemarketing + 
                 investmentAffiliates + investmentSEM + 
                 investmentOther, data = log_data_set)

summary(model_13)

#investmentSponsorship


model_14 <- lm(formula = gmv ~ Year +  
                 s1_fact.order_payment_type_COD + 
                 product_mrp_class_medium + product_mrp_class_premium + Event_Independence + 
                 investmentTV + investmentDigital + 
                 investmentContentMarketing + investmentOnlinemarketing + 
                 investmentAffiliates + investmentSEM + 
                 investmentOther, data = log_data_set)

summary(model_14)
#investmentAffiliates
model_15 <- lm(formula = gmv ~ Year +  
                 s1_fact.order_payment_type_COD + 
                 product_mrp_class_medium + product_mrp_class_premium + Event_Independence + 
                 investmentTV + investmentDigital + 
                 investmentContentMarketing + investmentOnlinemarketing + 
                 investmentSEM + 
                 investmentOther, data = log_data_set)

summary(model_15)
#investmentTV + investmentDigital + investmentContentMarketing + investmentOnlinemarketing +  investmentSEM + 

model_16 <- lm(formula = gmv ~ Year +  
                 s1_fact.order_payment_type_COD + 
                 product_mrp_class_medium + product_mrp_class_premium + Event_Independence + 
                 investmentOther, data = log_data_set)

summary(model_16)

#investmentOther

model_17 <- lm(formula = gmv ~ Year +  
                 s1_fact.order_payment_type_COD + 
                 product_mrp_class_medium + product_mrp_class_premium + Event_Independence
                 , data = log_data_set)

summary(model_17)


#Year

model_18 <- lm(formula = gmv ~  
                 s1_fact.order_payment_type_COD + 
                 product_mrp_class_medium + product_mrp_class_premium + Event_Independence
               , data = log_data_set)

summary(model_18)



Predict_1 <- predict(model_18,log_data_set)
log_data_set$test_gmv <- Predict_1
#View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(log_data_set$gmv,log_data_set$test_gmv)
rsquared <- cor(log_data_set$gmv,log_data_set$test_gmv)^2
rsquared
