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

## Reusable function to plot histograms for given data set and ith column 
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## Reusable function to plot box plots for given data set and ith column
plotBox <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x="", data)) + geom_boxplot()
  return (p)
}

plotBoxWithSegmentsForGVM <- function(data_in, i) {
  p <- ggplot(data=data_in, aes(x=factor(data_in[[i]]), gmv)) + 
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + 
    xlab(colnames(data_in)[i])
  return (p)
}

## Reusable function to plot bars for given data set and ith column
plotBar <- function(data_in, i) {
  ggplot(data_in, aes(x=data_in[[i]])) + 
    xlab(colnames(data_in)[i]) +
    theme(axis.text.x = element_text(face="plain", color="black", 
                                     size=9, angle=0, vjust=0),
          axis.text.y = element_text(face="plain", color="black", 
                                     size=9, angle=0)) +
    geom_bar() +
    geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9))
}

## Reusable function to plot in grid for given configuration of number of columns
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotBarForGmv <- function(data_in, col) {
  ggplot(data_in, aes(x=col, y = gmv)) + 
    geom_bar(stat = 'identity')
}

aggregateAndDoPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    grp <- paste0(colnames(data_in)[i]);
    p <- ggplot(data_in %>% group_by(grp = data_in[[i]]) %>% summarise(gmv=sum(gmv, na.rm=TRUE), na.rm=T) %>% arrange(desc(gmv)), aes(x=grp, y = gmv)) + 
      xlab(grp) +
      geom_bar(stat = 'identity')
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotCorrAgainstGmv <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], gmv = data_in$gmv)
  p <- ggplot(data, aes(x = x, y = gmv)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$gmv, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}

plotCorrAgainstRevenueGmv <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], gmv = data_in$gmv)
  p <- ggplot(data, aes(x = x, y = gmv)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$gmv, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}

## Reusable function to plot denisity for given data set and ith column
plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}

## End of Reusable functions for plots
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

##boxplot.stats(consumerElectronicsData$gmv)
##boxplot(consumerElectronicsData$gmv, outline = FALSE)

## End of load data sets
################################################################################################################################################

## Filter data for July 2015 to June 2016.

consumerElectronicsData$Month <- as.numeric(consumerElectronicsData$Month)
consumerElectronicsData$Year <- as.numeric(consumerElectronicsData$Year)
consumerElectronicsDataForAnalysis <- consumerElectronicsData %>% filter((Year == 2015 & Month >= 7) | (Year == 2016 & Month <= 6))
consumerElectronicsDataForAnalysis <- consumerElectronicsDataForAnalysis %>% filter(product_analytic_sub_category == "GamingAccessory" | product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio")
consumerElectronicsDataForAnalysis$offer_price = consumerElectronicsDataForAnalysis$product_mrp**consumerElectronicsDataForAnalysis$units - consumerElectronicsDataForAnalysis$gmv

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

## Numerical vs Categorical seperation

categorical_variables <- names(consumerElectronicsDataForAnalysis)[which(sapply(consumerElectronicsDataForAnalysis, is.character))]
numerical_variables <- names(consumerElectronicsDataForAnalysis)[which(sapply(consumerElectronicsDataForAnalysis, is.numeric))]
##numerical_variables = c(numerical_variables, names(consumerElectronicsDataForAnalysis)[which(sapply(consumerElectronicsDataForAnalysis, is.double))])

## Do column by column analysis and accordingly decide to remove or rearrange
categorical_variables <- categorical_variables[which(categorical_variables != "X.U.FEFF.fsn_id")]
categorical_variables <- categorical_variables[which(categorical_variables != "order_date")]
numerical_variables <- numerical_variables[which(numerical_variables != "Year")]
numerical_variables <- numerical_variables[which(numerical_variables != "Month")]
numerical_variables <- numerical_variables[which(numerical_variables != "order_id")]
numerical_variables <- numerical_variables[which(numerical_variables != "order_item_id")]
numerical_variables <- numerical_variables[which(numerical_variables != "cust_id")]
categorical_variables <- c(categorical_variables, "Year", "Month")

master_frame_categorical_variables_only <- consumerElectronicsDataForAnalysis[, categorical_variables]
master_frame_numerical_variables_only <- consumerElectronicsDataForAnalysis[, numerical_variables]

## Convert all character to factors
master_frame_categorical_variables_only[sapply(master_frame_categorical_variables_only, is.character)] <- 
  lapply(master_frame_categorical_variables_only[sapply(master_frame_categorical_variables_only, is.character)], 
         as.factor)

## End of Numerical vs Categorical seperation
################################################################################################################################################

## Univariate Analysis

## For all plots below, please zoom or full screen for better view

## Denstity plots for numeric variables
##doPlots(master_frame_numerical_variables_only, fun = plotDen, ii = 1:ncol(master_frame_numerical_variables_only), ncol = 5)

doPlots(master_frame_numerical_variables_only, fun = plotDen, ii = 1:ncol(master_frame_numerical_variables_only), ncol = 5)

## Bar plots each categorical variables.
doPlots(master_frame_numerical_variables_only, fun = plotBox, ii = 1:ncol(master_frame_numerical_variables_only), ncol = 2)

## End of Univariate Analysis
################################################################################################################################################

## Begin of Bivariate Analysis

##Lets remove columns X.U.FEFF.fsn_id, order_date and product_analytic_vertical for corrrelation plot (because so many products)

consumerElectronicsDataForAnalysisForCorrelation <- subset(consumerElectronicsDataForAnalysis, select = -c(X.U.FEFF.fsn_id, order_date, product_analytic_vertical))

dmy <- dummyVars(" ~ .", data = consumerElectronicsDataForAnalysisForCorrelation, fullRank=T)
consumerElectronicsDataForAnalysisWithDummayVariables <- data.frame(predict(dmy, newdata = consumerElectronicsDataForAnalysisForCorrelation))

correlationMatrix <- cor(consumerElectronicsDataForAnalysisWithDummayVariables, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## Plot scatter plot for variables that have high correlation.
highcorr <- c(names(correlationMatrix[,'gmv'])[which(correlationMatrix[,'gmv'] > 0.5)], names(correlationMatrix[,'gmv'])[which(correlationMatrix[,'gmv'] < -0.2)])
data_corr <- consumerElectronicsDataForAnalysisWithDummayVariables[,highcorr]
doPlots(data_corr, fun = plotCorrAgainstGmv, ii = 1:ncol(data_corr))

## End of Bivariate Analysis
################################################################################################################################################

# Boxplots of numeric variables relative to gmv
doPlots(cbind(master_frame_categorical_variables_only, gmv = consumerElectronicsDataForAnalysis$gmv), fun = plotBoxWithSegmentsForGVM, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 5)

################################################################################################################################################

aggregateAndDoPlots(cbind(master_frame_categorical_variables_only, gmv = consumerElectronicsDataForAnalysis$gmv), fun = plotBarForGmv, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 2)

################################################################################################################################################

## Lets do EDA against marketting budget

consumerElectronicsDataForAnalysis$order_date <- as.Date(consumerElectronicsDataForAnalysis$order_date)
consumerElectronicsDataForAnalysis$week <- as.numeric(format(consumerElectronicsDataForAnalysis$order_date,"%W"))
consumerElectronicsDataForAnalysis$day <- as.numeric(format(consumerElectronicsDataForAnalysis$order_date,"%d"))
consumerElectronicsDataForAnalysis <- distinct(consumerElectronicsDataForAnalysis)

consumerElectronicsDataForAnalysis <- merge(consumerElectronicsDataForAnalysis, salesEventsWeeklyLevel, by = c("Year", "Month", "week"), all.x = TRUE)

################################################################################################################################################

## Weekly aggregation

consumerElectronicsDataForAnalysisForAggregation <- subset(consumerElectronicsDataForAnalysis, select = -c(X.U.FEFF.fsn_id, order_date))

categorical_variables_indexes <- as.integer(which(sapply(consumerElectronicsDataForAnalysisForAggregation, is.character)))

weekAggregationSplit1 <- consumerElectronicsDataForAnalysisForAggregation %>% dplyr::select(Year, week, gmv, product_mrp, offer_price) %>% group_by(Year, week) %>% summarise_all(funs(sum), na.rm = TRUE)
weekAggregationSplit2 <- consumerElectronicsDataForAnalysisForAggregation %>% dplyr::select(Year, week, deliverycdays, deliverybdays, product_procurement_sla) %>% group_by(Year, week) %>% summarise_all(funs(mean), na.rm = TRUE)

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

consumerElectronicsDataForAnalysisWeeklyAggregation <- merge(consumerElectronicsDataForAnalysisWeeklyAggregation, npsWeeklyLevel, by = c("Year", "week"), all.x = TRUE)
consumerElectronicsDataForAnalysisWeeklyAggregation <- merge(consumerElectronicsDataForAnalysisWeeklyAggregation, budgetAllocationsWeekly, by = c("Year", "week"), all.x = TRUE)

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

## Begin of Bivariate Analysis (with included investment and at week level)

correlationMatrixWeekly <- cor(consumerElectronicsDataForAnalysisWeeklyAggregation, use = "pairwise.complete.obs")
##corrplot(correlationMatrixWeekly, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## Plot scatter plot for variables that have high correlation.
highcorrWeekly <- c(names(correlationMatrixWeekly[,'gmv'])[which(correlationMatrixWeekly[,'gmv'] > 0.7)], names(correlationMatrixWeekly[,'gmv'])[which(correlationMatrixWeekly[,'gmv'] < -0.7)])
data_corr_weekly <- consumerElectronicsDataForAnalysisWeeklyAggregation[,highcorrWeekly]
doPlots(data_corr_weekly, fun = plotCorrAgainstRevenueGmv, ii = 1:ncol(data_corr_weekly))

##---------------------------------------------------
## lets check only for investment against gmv
correlationMatrixWeeklyOnlyForInvestments <- cor(consumerElectronicsDataForAnalysisWeeklyAggregation %>% dplyr::select(union(starts_with("Investment"), starts_with("gmv"))), use = "pairwise.complete.obs")
corrplot(correlationMatrixWeeklyOnlyForInvestments, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## Plot scatter plot for variables that have high correlation.
highcorrWeeklyOnlyForInvestments <- c(names(correlationMatrixWeeklyOnlyForInvestments[,'gmv'])[which(correlationMatrixWeeklyOnlyForInvestments[,'gmv'] > 0.2)], names(correlationMatrixWeeklyOnlyForInvestments[,'gmv'])[which(correlationMatrixWeeklyOnlyForInvestments[,'gmv'] < -0.2)])
data_corr_weekly_only_for_investments <- consumerElectronicsDataForAnalysisWeeklyAggregation[,highcorrWeeklyOnlyForInvestments]
doPlots(data_corr_weekly_only_for_investments, fun = plotCorrAgainstRevenueGmv, ii = 1:ncol(data_corr_weekly_only_for_investments))
##---------------------------------------------------

dataset_final_analysis <- consumerElectronicsDataForAnalysisWeeklyAggregation
colnames(dataset_final_analysis)
#View(dataset_final_analysis)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataset_final_analysis[is.nan(dataset_final_analysis)] <- 0

set.seed(100)
trainindices= sample(1:nrow(dataset_final_analysis), 0.8*nrow(dataset_final_analysis))
train = dataset_final_analysis[trainindices,]
test = dataset_final_analysis[-trainindices,]

model_1 <-lm(formula = gmv ~ Year + SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentContentMarketing + investmentAffiliates + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               deliverybdays_mean + NPS_WeekAvg + investmentTV + 
               investmentSponsorship + investmentOnlinemarketing + investmentSEM 
               ,data=train)
summary(model_1)


vif(model_1)


#investmentSEM

model_2 <-lm(formula = gmv ~ Year + SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentContentMarketing + investmentAffiliates + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               deliverybdays_mean + NPS_WeekAvg + investmentTV + 
               investmentSponsorship + investmentOnlinemarketing 
             ,data=train)
summary(model_2)


vif(model_2)

#investmentContentMarketing

model_3 <-lm(formula = gmv ~ Year + SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentAffiliates + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               deliverybdays_mean + NPS_WeekAvg + investmentTV + 
               investmentSponsorship + investmentOnlinemarketing 
             ,data=train)
summary(model_3)


vif(model_3)

#deliverybdays_mean

model_4 <-lm(formula = gmv ~ Year + SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentAffiliates + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               NPS_WeekAvg + investmentTV + 
               investmentSponsorship + investmentOnlinemarketing 
             ,data=train)
summary(model_4)


vif(model_4)

#NPS_WeekAvg
model_5 <-lm(formula = gmv ~ Year + SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentAffiliates + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               investmentTV + 
               investmentSponsorship + investmentOnlinemarketing 
             ,data=train)
summary(model_5)


vif(model_5)

#Year

model_6 <-lm(formula = gmv ~ SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentAffiliates + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               investmentTV + 
               investmentSponsorship + investmentOnlinemarketing 
             ,data=train)
summary(model_6)


vif(model_6)

#investmentAffiliates
model_7 <-lm(formula = gmv ~ SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               investmentTV + 
               investmentSponsorship + investmentOnlinemarketing 
             ,data=train)
summary(model_7)

vif(model_7)


#investmentOnlinemarketing
model_8 <-lm(formula = gmv ~ SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + EventIndependence_sum_sum + 
               investmentTV + 
               investmentSponsorship  
             ,data=train)
summary(model_8)

vif(model_8)

#EventIndependence_sum_sum
model_9 <-lm(formula = gmv ~ SpecialEvent_sum_sum + 
               EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
               EventRepublic_sum_sum + deliverycdays_mean + investment + 
               investmentDigital + investmentRadio + 
               week + 
               EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + 
               investmentTV + 
               investmentSponsorship  
             ,data=train)
summary(model_9)

vif(model_9)
#deliverycdays_mean

model_10 <-lm(formula = gmv ~ SpecialEvent_sum_sum + 
                EventChristmas...New.Year_sum_sum + EventDussehra_sum_sum + EventFHSD_sum_sum + EventPacman_sum_sum +
                EventRepublic_sum_sum + investment + 
                investmentDigital + investmentRadio + 
                week + 
                EventBSD_sum_sum + EventDiwali_sum_sum + EventEid...Rathayatra_sum_sum + 
                investmentTV + 
                investmentSponsorship  
              ,data=train)
summary(model_10)

Predict_1 <- predict(model_10,test)
test$test_gmv <- Predict_1
#View(test)
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$gmv,test$test_gmv)
rsquared <- cor(test$gmv,test$test_gmv)^2
rsquared




