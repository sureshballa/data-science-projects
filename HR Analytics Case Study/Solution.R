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

load.libraries <- c('reshape', 'stringr', 'dplyr', 'data.table', 'e1071', 'gridExtra', 'corrplot', 'ggplot2', 'tidyr', 'MASS', 'car', 'caret', 'GGally', 'mice','cowplot','caTools')
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

## reusable function to plot segmented univariate analysis
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

## reusable function to plot segmented univariate analysis with stacked bar
plotSegmentedUniavriateAnalysisWithStackedBar <- function(data_in, i) {
  ggplot(data_in, aes(x = data_in[[i]], fill = Attrition)) + 
    labs(x = colnames(data_in)[i], y = "Percentage", fill = "Attrition") +
    geom_bar(position = "fill")
  ##scale_y_continuous(labels = percent_format())
}

## reusable function to plot boz plot against Attrition
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

## EnvironmentSatisfaction has 19 NA's for NumCompaniesWorked, 9 NA's for TotalWorkingYears, 25 NA's for EnvironmentSatisfaction, JobSatisfaction has 20 NA's, WorkLifeBalance has 38 NA's

md.pattern(master_frame)
#Imputing missing values using mice
mice_imputes = mice(master_frame, m=5, maxit = 40)
mice_imputes$method
## As expected pmm methods have been used
master_frame <- complete(mice_imputes)

## Lets confirm NA's again
colSums(is.na(master_frame))
colMeans(is.na(master_frame))
barplot(colMeans(is.na(master_frame)))

## No more NA's, we are good

## End of Handling of NA's
################################################################################################################################################

## Start of Binning of continous variables based on WOE and IV
## Age, Distance and TotalWorkingHours are eligible candidates for Binning based on WOE and IV
## Detailed analysis of why these groups have been included in presentation slide deck

## binning function for age
getAgeGroup <- function(age) {
  cut(age, breaks = c(8,25,30,35,50,60), labels = c("18-25", "26-30", "31-35", "36-50", "50-60"), include.lowest = TRUE, right = TRUE)
}

## binning function for distance from office
getDistanceGroup <- function(distance) {
  cut(distance, breaks = c(1,5,10,15,20,29), labels = c("1-5", "6-10", "11-15", "16-20", "21-29"), include.lowest = TRUE, right = TRUE)
}

## binning function for total working years
getTotalWorkingYearsGroup <- function(totalWorkingYears) {
  cut(totalWorkingYears, breaks = c(0,2,5,10,40), labels = c("0-2", "3-5", "6-10", "11-40"), include.lowest = TRUE, right = TRUE)
}

master_frame$AgeGroup <- sapply(as.numeric(master_frame$Age), getAgeGroup)
master_frame$DistanceGroup <- sapply(as.numeric(master_frame$DistanceFromHome), getDistanceGroup)
master_frame$TotalWorkingYearsGroup <- sapply(as.numeric(master_frame$TotalWorkingYears), getTotalWorkingYearsGroup)

## Lets make sure these are charcter types, becase down the line, we are seperating categorical vs numeric based on this
master_frame$AgeGroup <- as.character(master_frame$AgeGroup)
master_frame$DistanceGroup <- as.character(master_frame$DistanceGroup)
master_frame$TotalWorkingYearsGroup <- as.character(master_frame$TotalWorkingYearsGroup)

## Start of Binning of continous variables based on WOE and IV
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

## For all plots below, please zoom or full screen for better view

## Denstity plots for numeric variables
doPlots(master_frame_numerical_variables_only, fun = plotDen, ii = 1:ncol(master_frame_numerical_variables_only), ncol = 5)

## Bar plots each categorical variables.
doPlots(master_frame_categorical_variables_only, fun = plotBar, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 3)

## End of Univariate Analysis
################################################################################################################################################

## Begin of Segmented Univariate Analysis
## Bar plots of variables against attribution
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

## Also lets delete Age, DistanceFromHome and TotalWorkingYears becase we binned them
master_frame <- master_frame[,- which(colnames(master_frame)=='Age')]
master_frame <- master_frame[,- which(colnames(master_frame)=='DistanceFromHome')]
master_frame <- master_frame[,- which(colnames(master_frame)=='TotalWorkingYears')]

## Start of Dummy variables creation for categorical variables

## Note - dummyVars creates dummy variables for all character/factor, we want to avoid target variable,
## so converting Attrition to numeric and back to character after dummy variable creation process is done 
master_frame$Attrition <- ifelse(master_frame$Attrition=="Yes",1,0)
master_frame$Attrition <- as.numeric(master_frame$Attrition)
dmy <- dummyVars(" ~ .", data = master_frame, fullRank=T)
employee <- data.frame(predict(dmy, newdata = master_frame))
employee$Attrition <- as.numeric(master_frame$Attrition)

##Scale monthly incomes
employee$MonthlyIncome <- scale(employee$MonthlyIncome)

########################################################################
# splitting the data between train and test
set.seed(100)
indices = sample.split(employee$Attrition, SplitRatio = 0.7)
train = employee[indices,]
test = employee[!(indices),]

########################################################################

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition~ ., data = train, family = "binomial")
summary(model_1) #AIC 2133.8..coeff..nullDev 2728.0...resDev 2019.8

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

vif(model_2)

#Excluding YearsAtCompany due to low significance and comparitively higher VIF


model_3 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + DistanceFromHome + Education3 + Education4 + 
                 Education5 + EducationFieldMarketing + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 weekly_hours_avg + low_work_week_frequency + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
                 data = train)

summary(model_3)

vif(model_3)

#since all the comparitively higher VIF values left ex: BusinessTravelTravel_Frequently, BusinessTravelTravel_Rarely
#DepartmentSales shows high significance, we will now start removing variables based on p values.

#Excluding EducationFieldTechnical.Degree

model_5 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + DistanceFromHome + Education3 + Education4 + 
                 Education5 + EducationFieldMarketing + EducationFieldOther + 
                 JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 weekly_hours_avg + low_work_week_frequency + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
               data = train)

summary(model_5)

#Excluding low_work_week_frequency

model_6 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + DistanceFromHome + Education3 + Education4 + 
                 Education5 + EducationFieldMarketing + EducationFieldOther + 
                 JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 weekly_hours_avg + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
               data = train)

summary(model_6)

#Excluding EducationFieldMarketing

model_7 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + DistanceFromHome + Education3 + Education4 + 
                 Education5 + EducationFieldOther + 
                 JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 weekly_hours_avg + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
               data = train)

summary(model_7)

#Excluding EducationFieldOther

model_8 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + DistanceFromHome + Education3 + Education4 + 
                 Education5 + JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 weekly_hours_avg + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
               data = train)

summary(model_8)

#Excluding Education5
model_9 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + DistanceFromHome + Education3 + Education4 + 
                 JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 weekly_hours_avg + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
               data = train)

summary(model_9)

#Excluding Education3
model_10 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + DistanceFromHome + Education4 + 
                 JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 weekly_hours_avg + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
               data = train)

summary(model_10)

#Excluding Education4
model_11 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + DistanceFromHome + 
                  JobLevel2 + JobRoleLaboratory.Technician + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                  StockOptionLevel1 + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
                data = train)

summary(model_11)

#Excluding StockOptionLevel1

model_12 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + DistanceFromHome + 
                  JobLevel2 + JobRoleLaboratory.Technician + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
                data = train)

summary(model_12)

#Excluding DistanceFromHome

model_13 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobLevel2 + JobRoleLaboratory.Technician + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
                data = train)

summary(model_13)

#Excluding MonthlyIncome
model_14 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobLevel2 + JobRoleLaboratory.Technician + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3, family = "binomial", 
                data = train)

summary(model_14)

#Excluding JobInvolvement3 
model_15 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobLevel2 + JobRoleLaboratory.Technician + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)

summary(model_15)

#JobLevel2

model_16 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleLaboratory.Technician + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)

summary(model_16)

#Excluding JobRoleLaboratory.Technician

model_17 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)

summary(model_17)

#Excluding JobRoleResearch.Scientist

model_18 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales +  JobRoleResearch.Director + JobRoleSales.Executive + 
                  MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)

summary(model_18)

#Excluding JobRoleResearch.Director

model_19 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleSales.Executive + 
                  MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)

summary(model_19)

#Excluding JobRoleSales.Executive

model_20 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)

summary(model_20)

#Excluding JobSatisfaction3

model_21 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
            BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
            DepartmentSales + MaritalStatusSingle + NumCompaniesWorked + 
            TotalWorkingYears + TrainingTimesLastYear + 
            YearsSinceLastPromotion + YearsWithCurrManager + 
            weekly_hours_avg + EnvironmentSatisfaction2 + 
            EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
            JobSatisfaction4 + WorkLifeBalance2 + 
            WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
            data = train)

summary(model_21)

#Excluding JobSatisfaction2

model_22 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)

summary(model_22)

#Excluding WorkLifeBalance4
model_23 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 , family = "binomial", 
                data = train)

summary(model_23)

#Excluding WorkLifeBalance2
model_24 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + 
                  WorkLifeBalance3 , family = "binomial", 
                data = train)

summary(model_24)

#WorkLifeBalance3 is also being removed as it looks comparitely low significant
model_25 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 , family = "binomial", 
                data = train)

summary(model_25)

#TrainingTimesLastYear is also being removed as it looks comparitely low significant
model_26 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + MaritalStatusSingle + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  weekly_hours_avg + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 , family = "binomial", 
                data = train)

summary(model_26)

final_model<- model_26

### Model Evaluation

### Test Data ####

#predicted probabilities of AttritionYes for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's try use the probability cutoff of 50%.

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attr,test_pred_attr)

#######################################################################
test_pred_attr <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf

#########################################################################################

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)
s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of   for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.12, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")
conf_final

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

#################################################################################################
### KS -statistic - Test Data ######


test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test <- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attr, test_pred, groups = 10)
Churn_decile
