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
  p <- ggplot(data=data) + geom_boxplot()
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

consumerElectronicsData <- read.csv("ConsumerElectronics.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
budgetAllocations <- read.csv("budget_allocation.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))

colnames(budgetAllocations)[1] <- "Year"

## End of load data sets
################################################################################################################################################

## Filter data for July 2015 to June 2016.

consumerElectronicsData$Month <- as.numeric(consumerElectronicsData$Month)
consumerElectronicsData$Year <- as.numeric(consumerElectronicsData$Year)
consumerElectronicsDataForAnalysis <- consumerElectronicsData %>% filter((Year == 2015 & Month >= 7) | (Year == 2016 & Month <= 6))
##consumerElectronicsDataForAnalysis <- consumerElectronicsDataForAnalysis %>% filter(product_analytic_sub_category == "GamingAccessory" | product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio")

## Handling of NA's
NA.proportion <- function(x) mean(is.na(x))
table(NA.proportion=round(sapply(consumerElectronicsDataForAnalysis, NA.proportion), 2))

colSums(is.na(consumerElectronicsDataForAnalysis))
colMeans(is.na(consumerElectronicsDataForAnalysis))
barplot(colMeans(is.na(consumerElectronicsDataForAnalysis)))

## gmv, pincode and custid has null values
## TODO: Compute gmv for each product group. Ignore, cust id. And also ignore pincode for now (as per mentor)

## Lets confirm NA's again
colSums(is.na(consumerElectronicsDataForAnalysis))
colMeans(is.na(consumerElectronicsDataForAnalysis))
barplot(colMeans(is.na(consumerElectronicsDataForAnalysis)))

## No more NA's, we are good

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
doPlots(master_frame_numerical_variables_only, fun = plotDen, ii = 1:ncol(master_frame_numerical_variables_only), ncol = 5)

## Bar plots each categorical variables.
doPlots(master_frame_categorical_variables_only, fun = plotBar, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 2)

## End of Univariate Analysis
################################################################################################################################################

## Begin of Bivariate Analysis

##Lets remove columns X.U.FEFF.fsn_id, order_date and product_analytic_vertical for corrrelation plot (because so many products)

consumerElectronicsDataForAnalysisForCorrelation <- subset(consumerElectronicsDataForAnalysis, select = -c(X.U.FEFF.fsn_id, order_date, product_analytic_vertical))

dmy <- dummyVars(" ~ .", data = consumerElectronicsDataForAnalysisForCorrelation, fullRank=T)
consumerElectronicsDataForAnalysisWithDummayVariables <- data.frame(predict(dmy, newdata = consumerElectronicsDataForAnalysisForCorrelation))

correlationMatrix <- cor(consumerElectronicsDataForAnalysisWithDummayVariables, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## End of Bivariate Analysis
################################################################################################################################################

# Boxplots of numeric variables relative to gvm
doPlots(cbind(master_frame_categorical_variables_only, gvm = consumerElectronicsDataForAnalysis$gmv), fun = plotBoxWithSegmentsForGVM, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 5)


################################################################################################################################################

aggregateAndDoPlots(cbind(master_frame_categorical_variables_only, gmv = consumerElectronicsDataForAnalysis$gmv), fun = plotBarForGmv, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 2)

################################################################################################################################################

## Lets do EDA against marketting budget

consumerElectronicsDataForAnalysis$order_date <- as.Date(consumerElectronicsDataForAnalysis$order_date)
consumerElectronicsDataForAnalysis$week <- as.numeric(format(consumerElectronicsDataForAnalysis$order_date,"%W"))
consumerElectronicsDataForAnalysis$day <- as.numeric(format(consumerElectronicsDataForAnalysis$order_date,"%d"))

consumerElectronicsDataForAnalysisForAggregation <- subset(consumerElectronicsDataForAnalysis, select = -c(X.U.FEFF.fsn_id, order_date))
dmyForAggregation <- dummyVars(" ~ .", data = consumerElectronicsDataForAnalysisForAggregation, fullRank=T)
consumerElectronicsDataForAnalysisForAggregationWithDummayVariables <- data.frame(predict(dmyForAggregation, newdata = consumerElectronicsDataForAnalysisForAggregation))

dayAggregationSplit1 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(Year, Month, day, gmv, product_mrp) %>% group_by(Year, Month, day) %>% summarise_all(funs(revenue = sum), na.rm = TRUE)
dayAggregationSplit2 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(-c(gmv, order_id, order_item_id, sla, cust_id, pincode, week)) %>% group_by(Year, Month, day) %>% summarise_all(funs(mean = mean), na.rm = TRUE)
dayAggregationSplit3 <- consumerElectronicsDataForAnalysisForAggregationWithDummayVariables %>% dplyr::select(Year, Month, day, week) %>% group_by(Year, Month, day) %>% summarise(week = head(week, 1))

consumerElectronicsDataForAnalysisDayAggregation <- cbind(dayAggregationSplit1, dayAggregationSplit2, dayAggregationSplit3)

##consumerElectronicsDataForAnalysisDayAggregation <- consumerElectronicsDataForAnalysis %>% group_by(Year, Month, day) %>% summarise(n=n(), revenue=sum(gmv, na.rm=TRUE), subCategories = paste(unique(product_analytic_sub_category), collapse = ","), week = head(week, 1))

daysInMonth <- consumerElectronicsDataForAnalysisDayAggregation %>% group_by(Year, Month) %>% summarise(days=n())
budgetByMonths <- merge(daysInMonth, budgetAllocations, by= c("Year", "Month"))

computeInvestment <- function(record) {
  year <- as.numeric(record[1])
  month <- as.numeric(record[2])
  investment <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$Total.Investment
  days <- (budgetByMonths %>% filter(Year == year & Month == month) %>% head(1))$days
  return(investment/days)
}

consumerElectronicsDataForAnalysisDayAggregation$investment <- apply(consumerElectronicsDataForAnalysisDayAggregation, 1, computeInvestment)
consumerElectronicsDataForAnalysisDayAggregation$investment <-consumerElectronicsDataForAnalysisDayAggregation$investment * 10000000


################################################################################################################################################

## Side by side analysis of investment and revenue
## TODO: Updated variable names to have meaning full ones

weeklyRevenueVsInvestment <- consumerElectronicsDataForAnalysisDayAggregation %>% group_by(Year, week) %>% summarise(revenue = sum(gmv_revenue, na.rm=TRUE), investment = sum(investment, na.rm=TRUE))
temp <- as.data.frame((weeklyRevenueVsInvestment %>% filter(Year == 2015))[c(2,3,4)])
melted <- melt(temp, id.vars='week')

ggplot(melted, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

temp2 <- as.data.frame((weeklyRevenueVsInvestment %>% filter(Year == 2016))[c(2,3,4)])
melted2 <- melt(temp2, id.vars='week')

ggplot(melted2, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

################################################################################################################################################
