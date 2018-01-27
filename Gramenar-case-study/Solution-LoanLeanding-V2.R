## Begin of Reusable functions for plots

## To plot Histogram graphs 
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## To plot Box graphs
plotBox <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data) + geom_boxplot()
  return (p)
}

## To plot Bar graphs
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

# To plot bar graphs for univariant study
plotSegmentedUniavriateAnalysis <- function(data_in, dimension1_index, dimension2_index) {
  ggplot(data_in, aes(x = data_in[[dimension1_index]], fill = data_in[[dimension2_index]] )) + 
    labs(x = colnames(data_in)[dimension1_index], y = "Loans", fill = colnames(data_in)[dimension2_index]) +
    theme(axis.text.x = element_text(face="plain", color="black", 
                                     size=9, angle=0, vjust=0),
          axis.text.y = element_text(face="plain", color="black", 
                                     size=9, angle=0)) +
    geom_bar(alpha = 0.8, position = position_dodge(width = 0.8)) +
    geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9))
}

# To plot stacked bar graphs for univariant study
stackedbar <- function(data_in, dimension1_index, dimension2_index) {
  ggplot(data_in, aes(x = data_in[[dimension1_index]], fill = data_in[[dimension2_index]])) + 
    labs(x = colnames(data_in)[dimension1_index], y = "Loans", fill = colnames(data_in)[dimension2_index]) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent_format())
}



## 
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

## 
plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}
## End of Reusable functions for plots
##------------------------------------------------------------------------------------------------------------------------------

## Instruction to reviewer: Please make sure to set current working directory to same directory where all the files are present

## Install and load required libraries
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

## Load data set, handle null and NULL values
loan_data_set <- read.csv("loan.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","N/A","n/a","NaN","","#DIV/0!"))

#To understand the dataset
names(loan_data_set)
dim(loan_data_set)
head(loan_data_set)
summary(loan_data_set)
str(loan_data_set)

## Target variable
table(loan_data_set$loan_status)
prop.table(table(loan_data_set$loan_status))
barplot(prop.table(table(loan_data_set$loan_status)))

##pie chart showing loan status distribution
dist <- paste(names(table(loan_data_set$loan_status)), "\n", table(loan_data_set$loan_status), sep="")
pie(table(loan_data_set$loan_status), labels = dist, 
    main="split of loan status")

## Handling of NA's
NA.proportion <- function(x) mean(is.na(x))
table(NA.proportion=round(sapply(loan_data_set, NA.proportion), 2))

colSums(is.na(loan_data_set))
colMeans(is.na(loan_data_set))
barplot(colMeans(is.na(loan_data_set)))

## Delete columns with majority of missing values (NA's)
loan_data_set <- loan_data_set[, colMeans(is.na(loan_data_set)) <= 0.9]
dim(loan_data_set)
barplot(colMeans(is.na(loan_data_set)))

## Delete columns with majority of zeros as well
loan_data_set <- loan_data_set[, lapply(loan_data_set, function(x){ length(which(x==0))/length(x)}) <= 0.9]
dim(loan_data_set)

## Remove near zero variance variables which doesnt makese sense (For example, col having only one value is of no use)
## Based on observation, policy_code is always one. Lets remove technically
## Also, if we do not remove zero variance columns, 
## correlation matrix in Bivariate Analysis will have NA values and thus we cannot produce correlation heatmap graph
install.packages("caret")
library(caret)

nearZeroVariances <- nearZeroVar(loan_data_set, saveMetrics = TRUE)
nearZeroVariances_trues_indexes <- which(nearZeroVariances$nzv == TRUE)

if (length(nearZeroVariances_trues_indexes) > 0) {
  loan_data_set <- loan_data_set[, -(nearZeroVariances_trues_indexes)]
}


##--------------------------------------------------------------------------------------------------------------

## Univariate Analysis

## Numerical vs Categorical
categorical_variables = names(loan_data_set)[which(sapply(loan_data_set, is.character))]
numerical_variables = names(loan_data_set)[which(sapply(loan_data_set, is.numeric))]

loan_data_set_categorical_variables_only <- loan_data_set[, categorical_variables]
loan_data_set_numerical_variables_only <- loan_data_set[, numerical_variables]

loan_data_set_categorical_variables_only[sapply(loan_data_set_categorical_variables_only, is.character)] <- 
  lapply(loan_data_set_categorical_variables_only[sapply(loan_data_set_categorical_variables_only, is.character)], 
         as.factor)

## Denistity plots for numerica variables
doPlots(loan_data_set_numerical_variables_only, fun = plotDen, ii = 1:ncol(loan_data_set_numerical_variables_only), ncol = 5)

## Bar plots for loan status for each categorical variables.
## Team, this took more than one hour for me - please reduce the columns to meaning full ones
## doPlots(loan_data_set_categorical_variables_only, fun = plotBar, ii = 3:ncol(loan_data_set_categorical_variables_only), ncol = 5)
doPlots(loan_data_set_categorical_variables_only, fun = plotBar, ii = 3:8, ncol = 5)

## Segmented Univariate Analysis
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "grade"))
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "sub_grade"))
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "emp_length"))
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "home_ownership"), which(colnames(loan_data_set) == "loan_status"))

##--------------------------------------------------------------------------------------------------------------

## Bivariate Analysis
install.packages("corrplot")
library(corrplot)
correlationMatrix <- cor(loan_data_set_numerical_variables_only, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## Remove highly correlated columns, only keep one

##--------------------------------------------------------------------------------------------------------------
#To study indiviual variable for its influence for the probability to paid/charged off 

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#Employment Length#

chargedoff <- loan_data_set[loan_data_set$loan_status == "Charged Off",]
chargedoff <- chargedoff[chargedoff$emp_length != "n/a",]
table(chargedoff$emp_length)
prop.table(table(chargedoff$emp_length))
barplot(prop.table(table(chargedoff$emp_length)))
ggplot(chargedoff, aes(emp_length)) + 
  geom_bar(aes(x=reorder(emp_length,emp_length,
                         function(x)-length(x)),y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

# another view 
stackedbar(loan_data_set, which(colnames(loan_data_set) == "emp_length"), which(colnames(loan_data_set) == "loan_status"))


#Home Ownership#

ggplot(loan_data_set, aes(x = home_ownership,fill=factor(loan_status))) + geom_bar(position = "stack")
ggplot(chargedoff, aes(home_ownership)) + 
  geom_bar(aes(x=reorder(home_ownership,home_ownership,
                         function(x)-length(x)),y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

# another view
stackedbar(loan_data_set, which(colnames(loan_data_set) == "home_ownership"), which(colnames(loan_data_set) == "loan_status"))


#Loan Term#
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "term"), which(colnames(loan_data_set) == "loan_status"))
#loan term, i suggest to keep both#
stackedbar(loan_data_set, which(colnames(loan_data_set) == "term"), which(colnames(loan_data_set) == "loan_status"))

#Grade#
stackedbar(loan_data_set, which(colnames(loan_data_set) == "grade"), which(colnames(loan_data_set) == "loan_status"))

#Sub Grade#
stackedbar(loan_data_set, which(colnames(loan_data_set) == "sub_grade"), which(colnames(loan_data_set) == "loan_status"))

#Annual Income - bins of 30K#
#most loan are given to people who has salary upto 180K so this analsis is limited to that
getAnnualIncomeBuckets <- function(value) {
  cut(value, breaks = seq(0, max(loan_data_set$annual_inc)+30000,30000), labels = seq(0, max(loan_data_set$annual_inc),30000))
}

loan_data_set_subset <- subset(loan_data_set, loan_data_set$annual_inc< 180000)

loan_data_set_subset$AnnualIncomeClass <- sapply(loan_data_set_subset$annual_inc, getAnnualIncomeBuckets)

stackedbar(loan_data_set_subset, which(colnames(loan_data_set_subset) == "AnnualIncomeClass"), which(colnames(loan_data_set_subset) == "loan_status"))

#Loan amount - make bins of 10K#
getLoanAmtBuckets <- function(value) {
  cut(value, breaks = seq(0, max(loan_data_set$loan_amnt)+10000,10000), labels = seq(0, max(loan_data_set$loan_amnt),10000))
}

loan_data_set$loan_amntClass <- sapply(loan_data_set$loan_amnt, getLoanAmtBuckets)

stackedbar(loan_data_set, which(colnames(loan_data_set) == "loan_amntClass"), which(colnames(loan_data_set) == "loan_status"))

#Installment - make bins of 100
getInstallmentBuckets <- function(value) {
  cut(value, breaks = seq(0, max(loan_data_set$installment)+100,100), labels = seq(0, max(loan_data_set$installment),100))
}

loan_data_set$installmentClass <- sapply(loan_data_set$installment, getInstallmentBuckets)
stackedbar(loan_data_set, which(colnames(loan_data_set) == "installmentClass"), which(colnames(loan_data_set) == "loan_status"))

#Interest rate - make bins of 3
#library("stringr")
#remove the % sign
loan_data_set$int_rate<-str_replace(loan_data_set$int_rate, "%", c(""))

#convert from string to number
loan_data_set$int_rate<- as.numeric(loan_data_set$int_rate)

getInterestRateBuckets <- function(value) {
  cut(value, breaks = seq(0, max(loan_data_set$int_rate)+3,3), labels = seq(0, max(loan_data_set$int_rate),3))
}

loan_data_set$InterestRateClass <- sapply(loan_data_set$int_rate, getInterestRateBuckets)

stackedbar(loan_data_set, which(colnames(loan_data_set) == "InterestRateClass"), which(colnames(loan_data_set) == "loan_status"))

#Purpose# - plot indicates that small business contribute largly to charged off 
stackedbar(loan_data_set, which(colnames(loan_data_set) == "purpose"), which(colnames(loan_data_set) == "loan_status"))

#States
stackedbar(loan_data_set, which(colnames(loan_data_set) == "addr_state"), which(colnames(loan_data_set) == "loan_status"))

#impact of states on charged off - to see which states has more cases of charged off then others
ggplot(chargedoff, aes(addr_state)) + 
  geom_bar(aes(x=reorder(addr_state,addr_state,
                         function(x)-length(x)),y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

##--------------------------------------------------------------------------------------------------------------