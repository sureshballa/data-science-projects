## Team, please look into TODO statement. All these statements needs to be addressed before submission and 
## remove this communication statement and TODO action comments.
## Also, I have stubbed various sections to serve as reminder for us to complete the coding requirements


## Begin of Reusable functions for plots

## TODO: Explain this function 
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## TODO: Explain this function
plotBox <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data) + geom_boxplot()
  return (p)
}

## TODO: Explain this function
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

## TODO: Explain this function
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

## TODO: Explain this function
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
## TODO these staments aren't working for me, I had to manuall install and require these
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

## Load data set
loan_data_set <- read.csv("loan.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("NA","NaN","","#DIV/0!"))
names(loan_data_set)
dim(loan_data_set)
head(loan_data_set)
summary(loan_data_set)
str(loan_data_set)

## Target variable
table(loan_data_set$loan_status)
prop.table(table(loan_data_set$loan_status))
barplot(prop.table(table(loan_data_set$loan_status)))

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

## TODO: More data cleansing is required based on our analysis. 
## For example - title collum has 4 different values for personel 
## 'Personel loan', 'personel', 'My personel', 'Personel to help a friend', 'personel loan'
## We can combine all these values and make once category calling personel?

## TODO: Derive new columns based on existing columns.
## Case 1: Existing features are continous variables and need to bin these to brackets that makes sense for business
## For binning, in case if you would like, can refer my uber assignment solution. We can use cut function to have bins
## Case 2: Derive quarters, months from date variables? May be festive seasons, ppl take loans for buying products like car, start of education season for loan education

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

## ignore this - boxplot(loan_data_set, main = "boxplot.matrix(...., main = ...)", notch = TRUE, col = 1:dim(loan_data_set)[2] - 1)
## ignore this - ggplot(stack(loan_data_set), aes(x = ind, y = values)) + geom_boxplot()

## Denistity plots for numerica variables
doPlots(loan_data_set_numerical_variables_only, fun = plotDen, ii = 1:ncol(loan_data_set_numerical_variables), ncol = 5)

## Bar plots for loan status for each categorical variables.
## Team, this took more than one hour for me - please reduce the columns to meaning full ones
## doPlots(loan_data_set_categorical_variables_only, fun = plotBar, ii = 3:ncol(loan_data_set_categorical_variables_only), ncol = 5)
doPlots(loan_data_set_categorical_variables_only, fun = plotBar, ii = 3:8, ncol = 5)

## TODO: Segmented Univariate Analysis
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "grade"))
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "sub_grade"))
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "term"))
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "emp_length"))
plotSegmentedUniavriateAnalysis(loan_data_set, which(colnames(loan_data_set) == "loan_status"), which(colnames(loan_data_set) == "home_ownership"))

##--------------------------------------------------------------------------------------------------------------

## Bivariate Analysis

library(corrplot)
correlationMatrix <- cor(loan_data_set_numerical_variables_only, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## TODO: Remove highly correlated columns, only keep one

##--------------------------------------------------------------------------------------------------------------