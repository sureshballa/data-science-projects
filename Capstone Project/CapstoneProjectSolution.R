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
      ##geom_text(aes(label = ..identity.., y = ..identity..), stat= "identity", vjust = -0.3, position = position_dodge(width=0.9))
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

## End of load data sets
################################################################################################################################################

## Filter data for July 2015 to June 2016.

consumerElectronicsData$Month <- as.numeric(consumerElectronicsData$Month)
consumerElectronicsData$Year <- as.numeric(consumerElectronicsData$Year)
consumerElectronicsDataForAnalysis <- consumerElectronicsData %>% filter((Year == 2015 & Month >= 7) | (Year == 2016 & Month <= 6))
consumerElectronicsDataForAnalysis <- consumerElectronicsDataForAnalysis %>% filter(product_analytic_sub_category == "GamingAccessory" | product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio")

## Handling of NA's
NA.proportion <- function(x) mean(is.na(x))
table(NA.proportion=round(sapply(consumerElectronicsDataForAnalysis, NA.proportion), 2))

colSums(is.na(consumerElectronicsDataForAnalysis))
colMeans(is.na(consumerElectronicsDataForAnalysis))
barplot(colMeans(is.na(consumerElectronicsDataForAnalysis)))

# gmv, pincode and custid has null values

# md.pattern(consumerElectronicsDataForAnalysis)
# #Imputing missing values using mice
# mice_imputes = mice(consumerElectronicsDataForAnalysis, m=4, maxit = 40)
# mice_imputes$method
# ## As expected pmm methods have been used
# consumerElectronicsDataForAnalysis <- complete(mice_imputes)

## Lets confirm NA's again
colSums(is.na(consumerElectronicsDataForAnalysis))
colMeans(is.na(consumerElectronicsDataForAnalysis))
barplot(colMeans(is.na(consumerElectronicsDataForAnalysis)))

## Error: system is computationally singular: reciprocal condition number = 7.03375e-43

## No more NA's, we are good

## Remove near zero variance variables which doesnt makese sense (For example, col having only one value is of no use)
nearZeroVariances <- nearZeroVar(consumerElectronicsDataForAnalysis, saveMetrics = TRUE)
nearZeroVariances_trues_indexes <- which(nearZeroVariances$nzv == TRUE)

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

correlationMatrix <- cor(master_frame_numerical_variables_only, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method = "color", type = "lower", order = "FPC", tl.cex = 0.6)

## End of Bivariate Analysis
################################################################################################################################################

# Boxplots of numeric variables relative to gvm status
doPlots(cbind(master_frame_categorical_variables_only, gvm = consumerElectronicsDataForAnalysis$gmv), fun = plotBoxWithSegmentsForGVM, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 5)


################################################################################################################################################

##View(consumerElectronicsDataForAnalysis %>% group_by(product_analytic_sub_category) %>% summarise(gmv=sum(gmv, na.rm=TRUE), na.rm=T) %>% arrange(desc(gmv)))

aggregateAndDoPlots(cbind(master_frame_categorical_variables_only, gmv = consumerElectronicsDataForAnalysis$gmv), fun = plotBarForGmv, ii = 1:ncol(master_frame_categorical_variables_only), ncol = 2)

