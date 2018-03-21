## Instruction to reviewer: Please make sure to set current working directory to same directory
## where all the files are present

## IIITB - Group_Facilitator_RollNo: 
## Team:
## 1) Fayiz Mayam Veetil
## 2) Merin Jose
## 3) Deepak Aneja
## 4) Suresh Balla
################################################################################################################################################

## Business Objective
## TODO: Place verbiage from assignment page

## Begin of Install and load required libraries

load.libraries <- c('reshape', 'stringr', 'dplyr', 'data.table', 'e1071', 'gridExtra', 'corrplot', 'ggplot2', 'tidyr', 'MASS', 'car', 'caret', 'GGally')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

## End of Install and load required libraries
################################################################################################################################################

## Begin of Reusable functions for plots

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

## Reusable function to plot denisity for given data set and ith column
plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}

plotSegmentedUniavriateAnalysis <- function(data_in, i) {
  ggplot(data_in, aes(x = data_in[[i]], fill = Attrition )) + 
    labs(x = colnames(data_in)[i], y = "Count", fill = "Attrition") +
    theme(axis.text.x = element_text(face="plain", color="black", 
                                     size=9, angle=0, vjust=0),
          axis.text.y = element_text(face="plain", color="black", 
                                     size=9, angle=0)) +
    geom_bar(alpha = 0.8, position = position_dodge(width = 0.8)) +
    geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9))
}

plotSegmentedUniavriateAnalysisWithStackedBar <- function(data_in, i) {
  ggplot(data_in, aes(x = data_in[[i]], fill = Attrition)) + 
    labs(x = colnames(data_in)[i], y = "Percentage", fill = "Attrition") +
    geom_bar(position = "fill")
    ##scale_y_continuous(labels = percent_format())
}

plotBoxPlotsAgainstAttrition <- function(data_in, i) {
  p <- ggplot(data=data_in, aes(x = Attrition, y = data_in[[i]], fill = Attrition)) + geom_boxplot(width=0.2) + 
    labs(y = colnames(data_in)[i], fill = "Attrition") + coord_flip() + 
    theme(legend.position="none")
  return (p)
}

## End of Reusable functions for plots
################################################################################################################################################

## Load data sets

employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))

## End of load data sets
################################################################################################################################################


## Merge data sets and data prepation

## check for duplicates

if (length(general_data$EmployeeID) == length(unique(general_data$EmployeeID))) {
  print(paste0("No duplicates"))
}else {
  print(paste0("Duplicates present"))
}

if (length(employee_survey_data$EmployeeID) == length(unique(employee_survey_data$EmployeeID))) {
  print(paste0("No duplicates"))
}else {
  print(paste0("Duplicates present"))
}

if (length(manager_survey_data$EmployeeID) == length(unique(manager_survey_data$EmployeeID))) {
  print(paste0("No duplicates"))
}else {
  print(paste0("Duplicates present"))
}

if (length(in_time$X) == length(unique(in_time$X))) {
  print(paste0("No duplicates"))
}else {
  print(paste0("Duplicates present"))
}

if (length(out_time$X) == length(unique(out_time$X))) {
  print(paste0("No duplicates"))
}else {
  print(paste0("Duplicates present"))
}  

## Conclusion: No duplications in all data sets

## Assumption: Datasets in_time and out_time doesnt have column named for employee id. 
## Making assumption that first column is employee id.
in_time_melted <- melt(in_time, id=(c("X")))
out_time_melted <- melt(out_time, id=(c("X")))
employee_in_out_times <- merge(in_time_melted, out_time_melted, by = c("X", "variable"))
colnames(employee_in_out_times) <- c("EmployeeID", "Date", "InTime", "OutTime")
str(employee_in_out_times)
employee_in_out_times$Date <- as.character(employee_in_out_times$Date)
employee_in_out_times$Date <- str_replace(employee_in_out_times$Date, "X", c(""))
employee_in_out_times$Date <- as.Date(employee_in_out_times$Date, "%Y.%m.%d")
employee_in_out_times$InTime <- as.POSIXct(employee_in_out_times$InTime, "%Y-%m-%d %H:%M:%S")
employee_in_out_times$OutTime <- as.POSIXct(employee_in_out_times$OutTime, "%Y-%m-%d %H:%M:%S")
employee_in_out_times$Hours <- difftime(employee_in_out_times$OutTime, employee_in_out_times$InTime, units = "hours")
employee_in_out_times$Hours <- as.numeric(employee_in_out_times$Hours)
employee_in_out_times$year <- format(employee_in_out_times$Date,"%Y")
employee_in_out_times$month <- format(employee_in_out_times$Date,"%m")
employee_in_out_times$week <- format(employee_in_out_times$Date,"%W")

employee_time_aggregated_per_month <- employee_in_out_times %>% group_by(EmployeeID, year, month) %>% summarise(totalRecords = n(), total_hours=sum(Hours, na.rm = T), avg_hours_per_day=mean(Hours, na.rm = T))
employee_time_aggregated_weekly <- employee_in_out_times %>% group_by(EmployeeID, year, week) %>% summarise(totalRecords = n(), total_weekly_hours=sum(Hours, na.rm = T))
numberOfWeeksLoaded <- function(weekly_hours) {
  sum(weekly_hours >= 45)
}
numberOfWeeksWithLowWork <- function(weekly_hours) {
  sum(weekly_hours <= 10)
}
employee_time_aggregated <- employee_time_aggregated_weekly %>% group_by(EmployeeID) %>% summarise(weekly_hours_avg=mean(total_weekly_hours, na.rm = T), 
                                                                                                   loaded_week_frequency = numberOfWeeksLoaded(total_weekly_hours),
                                                                                                   low_work_week_frequency = numberOfWeeksWithLowWork(total_weekly_hours))

employee_data_with_office_hours <- merge(general_data, employee_time_aggregated, by="EmployeeID")
employee_data_with_office_hours_with_employee_survey_data <- merge(employee_data_with_office_hours, employee_survey_data, by="EmployeeID")
master_frame <- merge(employee_data_with_office_hours_with_employee_survey_data, manager_survey_data, by="EmployeeID")

## Emd of Merge data sets and data prepation

################################################################################################################################################

## Handling of NA's
NA.proportion <- function(x) mean(is.na(x))
table(NA.proportion=round(sapply(master_frame, NA.proportion), 2))

colSums(is.na(master_frame))
colMeans(is.na(master_frame))
barplot(colMeans(is.na(master_frame)))

## No major missing values, so no action required

## End of Handling of NA's
################################################################################################################################################

## Remove near zero variance variables which doesnt makese sense (For example, col having only one value is of no use)
nearZeroVariances <- nearZeroVar(master_frame, saveMetrics = TRUE)
nearZeroVariances_trues_indexes <- which(nearZeroVariances$nzv == TRUE)

if (length(nearZeroVariances_trues_indexes) > 0) {
  master_frame <- master_frame[, -(nearZeroVariances_trues_indexes)]
}

## Based on above operation, columns EmployeeCount, Over18 and StandardHours are removed becase these columns contains single value
################################################################################################################################################

## Numerical vs Categorical seperation

master_frame$StockOptionLevel <- as.character(master_frame$StockOptionLevel)
master_frame$EnvironmentSatisfaction <- as.character(master_frame$EnvironmentSatisfaction)
master_frame$JobSatisfaction <- as.character(master_frame$JobSatisfaction)
master_frame$JobInvolvement <- as.character(master_frame$JobInvolvement)
master_frame$PerformanceRating <- as.character(master_frame$PerformanceRating)
master_frame$WorkLifeBalance <- as.character(master_frame$WorkLifeBalance)
master_frame$Education = as.character(master_frame$Education)
master_frame$JobLevel = as.character(master_frame$JobLevel)

categorical_variables = names(master_frame)[which(sapply(master_frame, is.character))]
numerical_variables = names(master_frame)[which(sapply(master_frame, is.numeric))]

master_frame_categorical_variables_only <- master_frame[, categorical_variables]
master_frame_numerical_variables_only <- master_frame[, numerical_variables]

## Convert all character to factors
master_frame_categorical_variables_only[sapply(master_frame_categorical_variables_only, is.character)] <- 
  lapply(master_frame_categorical_variables_only[sapply(master_frame_categorical_variables_only, is.character)], 
         as.factor)

## End of Numerical vs Categorical seperation
################################################################################################################################################

## Univariate Analysis

## Denistity plots for numeric variables
doPlots(master_frame_numerical_variables_only, fun = plotDen, ii = 1:ncol(master_frame_numerical_variables_only), ncol = 5)

## Bar plots each categorical variables.
doPlots(master_frame_categorical_variables_only, fun = plotBar, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 3)

## End of Univariate Analysis
################################################################################################################################################

## Begin of Segmented Univariate Analysis
# Bar plots of variables against attribution
doPlots(master_frame_categorical_variables_only, fun = plotSegmentedUniavriateAnalysis, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 3)
doPlots(master_frame_categorical_variables_only, fun = plotSegmentedUniavriateAnalysisWithStackedBar, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 3)

# Boxplots of numeric variables relative to attrition status
doPlots(cbind(master_frame_numerical_variables_only, Attrition = master_frame$Attrition), fun = plotBoxPlotsAgainstAttrition, ii = 1:ncol(master_frame_numerical_variables_only), ncol = 3)

## End of Segmented Univariate Analysis
################################################################################################################################################

## Begin of Bivariate Analysis

correlationMatrix <- cor(master_frame_numerical_variables_only, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## End of Bivariate Analysis
################################################################################################################################################

## Start of modelling work

## Remove EmployeeId from master frame which is not required
master_frame <- master_frame[,- which(colnames(master_frame)=='EmployeeID')]

## Start of Dummy variables creation for categorical variables

## Note - dummyVars creates dummy variables for all character/factor, we want avoid target variable,
## so converting Attrition to numeric and back to character after dummy variable creation process is done 
master_frame$Attrition <- ifelse(master_frame$Attrition=="Yes",1,0)
master_frame$Attrition <- as.numeric(master_frame$Attrition)
dmy <- dummyVars(" ~ .", data = master_frame, fullRank=T)
master_frame_with_dummy_variables <- data.frame(predict(dmy, newdata = master_frame))
master_frame_with_dummy_variables$Attrition <- as.character(master_frame$Attrition)

## End of Dummy variables creation for categorical variables

##TODO: Scale age, monthly incomes, total_hours etc to avoid biased modelling


