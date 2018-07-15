## Instruction to reviewer: Please make sure to set correct path for s3 based files
bucket_path <- "s3://fayiz-bigdata-assignment/nyc-parking-case-study/";
filepath_2015 <- paste(bucket_path, "Parking_Violations_Issued_-_Fiscal_Year_2015.csv", sep = "")
filepath_2016 <- paste(bucket_path, "Parking_Violations_Issued_-_Fiscal_Year_2016.csv", sep = "")
filepath_2017 <- paste(bucket_path, "Parking_Violations_Issued_-_Fiscal_Year_2017.csv", sep = "")

## IIITB - Group_Facilitator_RollNo: DDA1730041
## Team:
## 1) Fayiz Mayam Veetil
## 2) Merin Jose
## 3) Deepak Aneja
## 4) Suresh Balla
################################################################################################################################################

# load SparkR
library(SparkR)

# initialise the spark session
sparkR.session(master='local')
# 2. Create a Spark DataFrame and examine structure
# reading a CSV file from S3 bucket for each year
parking_violations_issued_2015 <- SparkR::read.df(filepath_2015, header=T, "CSV", na.strings = c("NA","NaN","","#DIV/0!"))
parking_violations_issued_2016 <- SparkR::read.df(filepath_2016, header=T, "CSV", na.strings = c("NA","NaN","","#DIV/0!"))
parking_violations_issued_2017 <- SparkR::read.df(filepath_2017, header=T, "CSV", na.strings = c("NA","NaN","","#DIV/0!"))

# For using SQL, you need to create a temporary view
createOrReplaceTempView(parking_violations_issued_2015, "parking_violations_issued_2015_tbl")
createOrReplaceTempView(parking_violations_issued_2016, "parking_violations_issued_2016_tbl")
createOrReplaceTempView(parking_violations_issued_2017, "parking_violations_issued_2017_tbl")


################################ Examine the data ####################################
#------------------------------------------------------------------------------------------
#1) Find total number of tickets for each year.
# examine the size 2015
nrow(parking_violations_issued_2015)
#No. of Rows: 11809233
ncol(parking_violations_issued_2015)
#No. of Columns: 51
# examine the size 2016
nrow(parking_violations_issued_2016)
#No. of Rows: 10626899
ncol(parking_violations_issued_2016)
#No. of Columns: 51
# examine the size 2017
nrow(parking_violations_issued_2017)
#No. of Rows: 10803028
ncol(parking_violations_issued_2017)
#No. of Columns: 51

#Check for distinct Summons Number in the 2015 dataset
distictSummons2015 <- SparkR::sql(
  "SELECT count(distinct `Summons Number`)
  FROM parking_violations_issued_2015_tbl")
collect(distictSummons2015)
#distinct summons number = 10951256 which is not matching with actual number of rows in data file (11809233)

#check the top 5 duplicate summons number
duplicateSummons2015 <- SparkR::sql(
  "SELECT `Summons Number`, COUNT(*)
  FROM parking_violations_issued_2015_tbl
  GROUP BY `Summons Number`
  ORDER BY COUNT(*) DESC LIMIT 5")
collect(duplicateSummons2015)
#Summons Number count(1)
#1     1361384190       11
#2     1371684200       11
#3     1363207362       11
#4     1362490957       11
#5     1366303180       11

#check the if all rows are same for summons number = 1361384190
duplicateRowCheckFor1361384190 <- SparkR::sql(
  "SELECT * FROM parking_violations_issued_2015_tbl
  WHERE `Summons Number` = 1361384190")
collect(duplicateRowCheckFor1361384190)
#check the if all rows are same for summons number = 1371684200
duplicateRowCheckFor1371684200 <- SparkR::sql(
  "SELECT * FROM parking_violations_issued_2015_tbl
  WHERE `Summons Number` = 1371684200")
collect(duplicateRowCheckFor1371684200)
#check the if all rows are same for summons number = 1363207362
duplicateRowCheckFor1363207362 <- SparkR::sql(
  "SELECT * FROM parking_violations_issued_2015_tbl
  WHERE `Summons Number` = 1363207362")
collect(duplicateRowCheckFor1363207362)
#seems all values are same for all duplicated records - so assuming there are duplicates records in 2015

#apply drop duplicate functions to remove the duplicate records from 2015 and reassign new dataset to the variable
parking_violations_issued_2015 <- dropDuplicates(parking_violations_issued_2015, 'Summons Number')
nrow(parking_violations_issued_2015)
#No. of Rows: 10951256

#Check for distinct Summons Number in the 2016 dataset
distictSummons2016 <- SparkR::sql(
  "SELECT count(distinct `Summons Number`)
  FROM parking_violations_issued_2016_tbl")
collect(distictSummons2016)
#distinct summons number = 10626899 which is not matching with actual number of rows in data file
#seems no duplicate records in 2016 data set so further check required

#Check for distinct Summons Number in the 2017 dataset
distictSummons2017 <- SparkR::sql(
  "SELECT count(distinct `Summons Number`)
  FROM parking_violations_issued_2017_tbl")
collect(distictSummons2017)
#distinct summons number = 10803028  which is not matching with actual number of rows in data file
#seems no duplicate records in 2017 data set so further check required

#1) Find total number of tickets for each year.
collect(distictSummons2015)
#2015 = 10951256
collect(distictSummons2016)
#2016 = 10626899
collect(distictSummons2017)
#2017 = 10803028


# Reassign the temp view as there was changes in 2015 dataset
createOrReplaceTempView(parking_violations_issued_2015, "parking_violations_issued_2015_tbl")


#-----------------------------------------------------------------------------------------
#2) Find out how many unique states the cars which got parking tickets came from.
uniqueStates2015 <- SparkR::sql(
  "SELECT count(distinct `Registration State`)
  FROM parking_violations_issued_2015_tbl")
collect(uniqueStates2015)
## Unique States in 2015: 69
uniqueStates2016 <- SparkR::sql(
  "SELECT count(distinct `Registration State`)
  FROM parking_violations_issued_2016_tbl")
collect(uniqueStates2016)
## Unique States in 2016: 68
uniqueStates2017 <- SparkR::sql(
  "SELECT count(distinct `Registration State`)
  FROM parking_violations_issued_2017_tbl")
collect(uniqueStates2017)
## Unique States in 2017: 67

#------------------------------------------------------------------------------------------
#3) Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are.
nrow(filter(parking_violations_issued_2015, isNull(parking_violations_issued_2015$`Street Name`) | parking_violations_issued_2015$`Street Name` == '' | isNull(parking_violations_issued_2015$`House Number`) | parking_violations_issued_2015$`House Number` == ''))
## Parking tickets without address 2015: 1807864
nrow(filter(parking_violations_issued_2016, isNull(parking_violations_issued_2016$`Street Name`) | parking_violations_issued_2016$`Street Name` == '' | isNull(parking_violations_issued_2016$`House Number`) | parking_violations_issued_2016$`House Number` == ''))
## Parking tickets without address 2016: 2035232
nrow(filter(parking_violations_issued_2017, isNull(parking_violations_issued_2017$`Street Name`) | parking_violations_issued_2017$`Street Name` == '' | isNull(parking_violations_issued_2017$`House Number`) | parking_violations_issued_2017$`House Number` == ''))
## Parking tickets without address 2017: 2289944
################################### Examine the data ###########################################

################################### Aggregation tasks ##########################################
#--------------------------------------------------------------------------------------------
#1) How often does each violation code occur? (frequency of violation codes - find the top 5)
##Year 2015
voilationsByCode2015 <- SparkR::sql(
  "SELECT `Violation Code`, COUNT(*) 
  FROM parking_violations_issued_2015_tbl 
  GROUP BY `Violation Code` 
  ORDER BY COUNT(*) DESC LIMIT 5")
collect(voilationsByCode2015)
##Resultset 2015
#1             21  1501614
#2             38  1324586
#3             14   924627
#4             36   761571
#5             37   746278

##Year 2016
voilationsByCode2016 <- SparkR::sql(
  "SELECT `Violation Code`, COUNT(*) 
  FROM parking_violations_issued_2016_tbl 
  GROUP BY `Violation Code` 
  ORDER BY COUNT(*) DESC LIMIT 5")
collect(voilationsByCode2016)
##Resultset 2016
#1             21  1531587
#2             36  1253512
#3             38  1143696
#4             14   875614
#5             37   686610

##Year 2017
voilationsByCode2017 <- SparkR::sql(
  "SELECT `Violation Code`, COUNT(*) 
  FROM parking_violations_issued_2017_tbl 
  GROUP BY `Violation Code` 
  ORDER BY COUNT(*) DESC LIMIT 5")
collect(voilationsByCode2017)
##Resultset 2017
#1             21  1528588
#2             36  1400614
#3             38  1062304
#4             14   893498
#5             20   618593

#-------------------------------------------------------------------------------------------------------------------
#2) How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)
##Violation By Body Type
##Year 2015
voilationsByBodyType2015 <- SparkR::sql(
  "SELECT `Vehicle Body Type`, COUNT(*) 
  FROM parking_violations_issued_2015_tbl 
  GROUP BY `Vehicle Body Type` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByBodyType2015)
#Violation By Body Type - 2015
#1              SUBN  3451963
#2              4DSD  3102510
#3               VAN  1605228
#4              DELV   840441
#5               SDN   453992

##Year 2016
voilationsByBodyType2016 <- SparkR::sql(
  "SELECT `Vehicle Body Type`, COUNT(*) 
  FROM parking_violations_issued_2016_tbl 
  GROUP BY `Vehicle Body Type` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByBodyType2016)
#Violation By Body Type - 2016
#1              SUBN  3466037
#2              4DSD  2992107
#3               VAN  1518303
#4              DELV   755282
#5               SDN   424043


##Year 2017
voilationsByBodyType2017 <- SparkR::sql(
  "SELECT `Vehicle Body Type`, COUNT(*) 
  FROM parking_violations_issued_2017_tbl 
  GROUP BY `Vehicle Body Type` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByBodyType2017)
#Violation By Body Type - 2017
#1              SUBN  3719802
#2              4DSD  3082020
#3               VAN  1411970
#4              DELV   687330
#5               SDN   438191

##Violation By Vehicle Make
##Year 2015
voilationsByMake2015 <- SparkR::sql(
  "SELECT `Vehicle Make`, COUNT(*) 
  FROM parking_violations_issued_2015_tbl 
  GROUP BY `Vehicle Make` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByMake2015)
#Violation By Vehicle Make - 2015
#1         FORD  1417303
#2        TOYOT  1123523
#3        HONDA  1018049
#4        NISSA   837569
#5        CHEVR   836389

##Year 2016
voilationsByMake2016 <- SparkR::sql(
  "SELECT `Vehicle Make`, COUNT(*) 
  FROM parking_violations_issued_2016_tbl 
  GROUP BY `Vehicle Make` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByMake2016)
#Violation By Vehicle Make - 2016
#1         FORD  1324774
#2        TOYOT  1154790
#3        HONDA  1014074
#4        NISSA   834833
#5        CHEVR   759663

##Year 2017
voilationsByMake2017 <- SparkR::sql(
  "SELECT `Vehicle Make`, COUNT(*) 
  FROM parking_violations_issued_2017_tbl 
  GROUP BY `Vehicle Make` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByMake2017)
#Violation By Vehicle Make - 2017
#1         FORD  1280958
#2        TOYOT  1211451
#3        HONDA  1079238
#4        NISSA   918590
#5        CHEVR   714655

#-------------------------------------------------------------------------------------------------------------------
#3) A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
####1) Violating Precincts (this is the precinct of the zone where the violation occurred)
#Year - 2015
voilationsByViolatingPrecincts2015 <- SparkR::sql(
  "SELECT `Violation Precinct`, COUNT(*) 
  FROM parking_violations_issued_2015_tbl 
  GROUP BY `Violation Precinct` 
  ORDER BY COUNT(*) DESC
  LIMIT 5")
collect(voilationsByViolatingPrecincts2015)
#Violation Precinct - 2015
#1                  0  1633006
#2                 19   559716
#3                 18   400887
#4                 14   384596
#5                  1   307808

#Year - 2016
voilationsByViolatingPrecincts2016 <- SparkR::sql(
  "SELECT `Violation Precinct`, COUNT(*) 
  FROM parking_violations_issued_2016_tbl 
  GROUP BY `Violation Precinct` 
  ORDER BY COUNT(*) DESC
  LIMIT 5")
collect(voilationsByViolatingPrecincts2016)
#Violation Precinct - 2016
#1                  0  1868655
#2                 19   554465
#3                 18   331704
#4                 14   324467
#5                  1   303850

#Year - 2017
voilationsByViolatingPrecincts2017 <- SparkR::sql(
  "SELECT `Violation Precinct`, COUNT(*) 
  FROM parking_violations_issued_2017_tbl 
  GROUP BY `Violation Precinct` 
  ORDER BY COUNT(*) DESC
  LIMIT 5")
collect(voilationsByViolatingPrecincts2017)
#Violation Precinct - 2017
#1                  0  2072400
#2                 19   535671
#3                 14   352450
#4                  1   331810
#5                 18   306920

####2) Issuing Precincts (this is the precinct that issued the ticket)
#Year - 2015
voilationsByIssuerPrecincts2015 <- SparkR::sql(
  "SELECT `Issuer Precinct`, COUNT(*) 
  FROM parking_violations_issued_2015_tbl 
  GROUP BY `Issuer Precinct` 
  ORDER BY COUNT(*) DESC
  LIMIT 5")
collect(voilationsByIssuerPrecincts2015)
#Issuer Precinct - 2015
#1               0  1834343
#2              19   544946
#3              18   391501
#4              14   369725
#5               1   298594

#Year - 2016
voilationsByIssuerPrecincts2016 <- SparkR::sql(
  "SELECT `Issuer Precinct`, COUNT(*) 
  FROM parking_violations_issued_2016_tbl 
  GROUP BY `Issuer Precinct` 
  ORDER BY COUNT(*) DESC
  LIMIT 5")
collect(voilationsByIssuerPrecincts2016)
#Issuer Precinct - 2016
#1               0  2140274
#2              19   540569
#3              18   323132
#4              14   315311
#5               1   295013

#Year - 2017
voilationsByIssuerPrecincts2017 <- SparkR::sql(
  "SELECT `Issuer Precinct`, COUNT(*) 
  FROM parking_violations_issued_2017_tbl 
  GROUP BY `Issuer Precinct` 
  ORDER BY COUNT(*) DESC
  LIMIT 5")
collect(voilationsByIssuerPrecincts2017)
#Issuer Precinct - 2017
#1               0  2388479
#2              19   521513
#3              14   344977
#4               1   321170
#5              18   296553

#-----------------------------------------------------------------------------------------------------------------------------------------
#4) Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?
#Year 2015
voilationsByViolatingPrecinctsAndVoilationCodes2015 <- SparkR::sql(
  "SELECT `Violation Precinct`, `Violation Code`, COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl 
  WHERE `Violation Precinct` IN (
    SELECT `Violation Precinct` 
    FROM parking_violations_issued_2015_tbl 
    GROUP BY `Violation Precinct` 
    ORDER BY COUNT(*) DESC 
    LIMIT 3)
  GROUP BY `Violation Precinct`, `Violation Code`
  ORDER BY `Violation Precinct`, COUNT DESC")
collect(voilationsByViolatingPrecinctsAndVoilationCodes2015)
#Top 3 Precincts which issued most no. of tickets in 2015 are 0, 18, 19
#other anlysis are explained in the presentation files


#Year 2016
voilationsByViolatingPrecinctsAndVoilationCodes2016 <- SparkR::sql(
  "SELECT `Violation Precinct`, `Violation Code`, COUNT(*) AS COUNT
  FROM parking_violations_issued_2016_tbl 
  WHERE `Violation Precinct` IN (
    SELECT `Violation Precinct` 
    FROM parking_violations_issued_2016_tbl 
    GROUP BY `Violation Precinct` 
    ORDER BY COUNT(*) DESC 
    LIMIT 3)
  GROUP BY `Violation Precinct`, `Violation Code`
  ORDER BY `Violation Precinct`, COUNT DESC")
collect(voilationsByViolatingPrecinctsAndVoilationCodes2016)
#Top 3 Precincts which issued most no. of tickets in 2015 are 0, 18, 19
#other anlysis are explained in the presentation files

#Year 2017
voilationsByViolatingPrecinctsAndVoilationCodes2017 <- SparkR::sql(
  "SELECT `Violation Precinct`, `Violation Code`, COUNT(*) AS COUNT
  FROM parking_violations_issued_2017_tbl 
  WHERE `Violation Precinct` IN (
    SELECT `Violation Precinct` 
    FROM parking_violations_issued_2017_tbl 
    GROUP BY `Violation Precinct` 
    ORDER BY COUNT(*) DESC 
    LIMIT 3)
  GROUP BY `Violation Precinct`, `Violation Code`
  ORDER BY `Violation Precinct`, COUNT DESC")
collect(voilationsByViolatingPrecinctsAndVoilationCodes2017)
#Top 3 Precincts which issued most no. of tickets in 2015 are 0, 19, 14
#other anlysis are explained in the presentation files

#-----------------------------------------------------------------------------------------------------------------------------------------------
#Q5))))
## You’d want to find out the properties of parking violations across different times of the day:
## The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
## Find a way to deal with missing values, if any.
## Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the 3 most commonly occurring violations
## Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)


## The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
## Find a way to deal with missing values, if any.
## Lets check missing values
#Year 2015
voilationsNullsPercentageForEachColumns2015 <- SparkR::sql(
  "SELECT 
  SUM(IF(`Summons Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Summons Number`,
  SUM(IF(`Plate ID` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate ID`,
  SUM(IF(`Registration State` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Registration State`,
  SUM(IF(`Plate Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate Type`,
  SUM(IF(`Issue Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issue Date`,
  SUM(IF(`Violation Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Code`,
  SUM(IF(`Vehicle Body Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Body Type`,
  SUM(IF(`Vehicle Make` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Make`,
  SUM(IF(`Issuing Agency` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuing Agency`,
  SUM(IF(`Street Code1` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code1`,
  SUM(IF(`Street Code2` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code2`,
  SUM(IF(`Street Code3` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code3`,
  SUM(IF(`Vehicle Expiration Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Expiration Date`,
  SUM(IF(`Violation Location` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Location`,
  SUM(IF(`Violation Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Precinct`,
  SUM(IF(`Issuer Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Precinct`,
  SUM(IF(`Issuer Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Code`,
  SUM(IF(`Issuer Command` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Command`,
  SUM(IF(`Issuer Squad` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Squad`,
  SUM(IF(`Violation Time` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Time`,
  SUM(IF(`Time First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Time First Observed`,
  SUM(IF(`Violation County` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation County`,
  SUM(IF(`Violation In Front Of Or Opposite` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation In Front Of Or Opposite`,
  SUM(IF(`House Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage House Number`,
  SUM(IF(`Street Name` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Name`,
  SUM(IF(`Intersecting Street` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Intersecting Street`,
  SUM(IF(`Date First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Date First Observed`,
  SUM(IF(`Law Section` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Law Section`,
  SUM(IF(`Sub Division` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Sub Division`,
  SUM(IF(`Violation Legal Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Legal Code`,
  SUM(IF(`Days Parking In Effect    ` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Days Parking In Effect`,
  SUM(IF(`From Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage From Hours In Effect`,
  SUM(IF(`To Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage To Hours In Effect`,
  SUM(IF(`Vehicle Color` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Color`,
  SUM(IF(`Unregistered Vehicle?` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Unregistered Vehicle?`,
  SUM(IF(`Vehicle Year` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Year`,
  SUM(IF(`Meter Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Meter Number`,
  SUM(IF(`Feet From Curb` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Feet From Curb`,
  SUM(IF(`Violation Post Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Post Code`,
  SUM(IF(`Violation Description` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Description`,
  SUM(IF(`No Standing or Stopping Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage No Standing or Stopping Violation`,
  SUM(IF(`Hydrant Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Hydrant Violation`,
  SUM(IF(`Double Parking Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Double Parking Violation`,
  SUM(IF(`Latitude` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Latitude`,
  SUM(IF(`Longitude` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Longitude`,
  SUM(IF(`Community Board` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Community Board`,
  SUM(IF(`Community Council ` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage`,
  SUM(IF(`Census Tract` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Census Tract`,
  SUM(IF(`BIN` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage BIN`,
  SUM(IF(`BBL` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage BBL`,
  SUM(IF(`NTA` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage NTA`
  FROM parking_violations_issued_2015_tbl"
)
collect(voilationsNullsPercentageForEachColumns2015)

#Year 2016
voilationsNullsPercentageForEachColumns2016 <- SparkR::sql(
  "SELECT 
  SUM(IF(`Summons Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Summons Number`,
  SUM(IF(`Plate ID` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate ID`,
  SUM(IF(`Registration State` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Registration State`,
  SUM(IF(`Plate Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate Type`,
  SUM(IF(`Issue Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issue Date`,
  SUM(IF(`Violation Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Code`,
  SUM(IF(`Vehicle Body Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Body Type`,
  SUM(IF(`Vehicle Make` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Make`,
  SUM(IF(`Issuing Agency` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuing Agency`,
  SUM(IF(`Street Code1` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code1`,
  SUM(IF(`Street Code2` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code2`,
  SUM(IF(`Street Code3` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code3`,
  SUM(IF(`Vehicle Expiration Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Expiration Date`,
  SUM(IF(`Violation Location` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Location`,
  SUM(IF(`Violation Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Precinct`,
  SUM(IF(`Issuer Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Precinct`,
  SUM(IF(`Issuer Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Code`,
  SUM(IF(`Issuer Command` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Command`,
  SUM(IF(`Issuer Squad` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Squad`,
  SUM(IF(`Violation Time` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Time`,
  SUM(IF(`Time First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Time First Observed`,
  SUM(IF(`Violation County` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation County`,
  SUM(IF(`Violation In Front Of Or Opposite` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation In Front Of Or Opposite`,
  SUM(IF(`House Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage House Number`,
  SUM(IF(`Street Name` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Name`,
  SUM(IF(`Intersecting Street` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Intersecting Street`,
  SUM(IF(`Date First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Date First Observed`,
  SUM(IF(`Law Section` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Law Section`,
  SUM(IF(`Sub Division` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Sub Division`,
  SUM(IF(`Violation Legal Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Legal Code`,
  SUM(IF(`Days Parking In Effect    ` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Days Parking In Effect`,
  SUM(IF(`From Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage From Hours In Effect`,
  SUM(IF(`To Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage To Hours In Effect`,
  SUM(IF(`Vehicle Color` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Color`,
  SUM(IF(`Unregistered Vehicle?` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Unregistered Vehicle?`,
  SUM(IF(`Vehicle Year` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Year`,
  SUM(IF(`Meter Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Meter Number`,
  SUM(IF(`Feet From Curb` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Feet From Curb`,
  SUM(IF(`Violation Post Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Post Code`,
  SUM(IF(`Violation Description` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Description`,
  SUM(IF(`No Standing or Stopping Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage No Standing or Stopping Violation`,
  SUM(IF(`Hydrant Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Hydrant Violation`,
  SUM(IF(`Double Parking Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Double Parking Violation`,
  SUM(IF(`Latitude` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Latitude`,
  SUM(IF(`Longitude` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Longitude`,
  SUM(IF(`Community Board` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Community Board`,
  SUM(IF(`Community Council ` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage`,
  SUM(IF(`Census Tract` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Census Tract`,
  SUM(IF(`BIN` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage BIN`,
  SUM(IF(`BBL` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage BBL`,
  SUM(IF(`NTA` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage NTA`
  FROM parking_violations_issued_2016_tbl"
)
collect(voilationsNullsPercentageForEachColumns2016)

#Year 2017
voilationsNullsPercentageForEachColumns2017 <- SparkR::sql(
  "SELECT 
  SUM(IF(`Summons Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Summons Number`,
  SUM(IF(`Plate ID` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate ID`,
  SUM(IF(`Registration State` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Registration State`,
  SUM(IF(`Plate Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate Type`,
  SUM(IF(`Issue Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issue Date`,
  SUM(IF(`Violation Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Code`,
  SUM(IF(`Vehicle Body Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Body Type`,
  SUM(IF(`Vehicle Make` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Make`,
  SUM(IF(`Issuing Agency` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuing Agency`,
  SUM(IF(`Street Code1` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code1`,
  SUM(IF(`Street Code2` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code2`,
  SUM(IF(`Street Code3` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code3`,
  SUM(IF(`Vehicle Expiration Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Expiration Date`,
  SUM(IF(`Violation Location` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Location`,
  SUM(IF(`Violation Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Precinct`,
  SUM(IF(`Issuer Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Precinct`,
  SUM(IF(`Issuer Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Code`,
  SUM(IF(`Issuer Command` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Command`,
  SUM(IF(`Issuer Squad` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Squad`,
  SUM(IF(`Violation Time` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Time`,
  SUM(IF(`Time First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Time First Observed`,
  SUM(IF(`Violation County` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation County`,
  SUM(IF(`Violation In Front Of Or Opposite` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation In Front Of Or Opposite`,
  SUM(IF(`House Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage House Number`,
  SUM(IF(`Street Name` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Name`,
  SUM(IF(`Intersecting Street` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Intersecting Street`,
  SUM(IF(`Date First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Date First Observed`,
  SUM(IF(`Law Section` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Law Section`,
  SUM(IF(`Sub Division` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Sub Division`,
  SUM(IF(`Violation Legal Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Legal Code`,
  SUM(IF(`Days Parking In Effect    ` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Days Parking In Effect`,
  SUM(IF(`From Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage From Hours In Effect`,
  SUM(IF(`To Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage To Hours In Effect`,
  SUM(IF(`Vehicle Color` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Color`,
  SUM(IF(`Unregistered Vehicle?` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Unregistered Vehicle?`,
  SUM(IF(`Vehicle Year` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Year`,
  SUM(IF(`Meter Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Meter Number`,
  SUM(IF(`Feet From Curb` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Feet From Curb`,
  SUM(IF(`Violation Post Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Post Code`,
  SUM(IF(`Violation Description` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Description`,
  SUM(IF(`No Standing or Stopping Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage No Standing or Stopping Violation`,
  SUM(IF(`Hydrant Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Hydrant Violation`,
  SUM(IF(`Double Parking Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Double Parking Violation`
  FROM parking_violations_issued_2017_tbl"
)
collect(voilationsNullsPercentageForEachColumns2017)


#-----------------------------------------------------------------------------------------------------------------------------------------------
##TODO: What should we do to impute missing values

#-----------------------------------------------------------------------------------------------------------------------------------------------

##c Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the 3 most commonly occurring violations

##NA's in the result sets are kept sepearatley as it's not logical to add them in any of the time bin, unless otherwise specified

voilationsByTimeBins2015 <- SparkR::sql(
  "SELECT COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '20-24'
  END AS Time_Bin,
  `Violation Code`
  FROM parking_violations_issued_2015_tbl 
  GROUP BY
    Time_Bin, `Violation Code`
  ORDER BY Time_Bin, COUNT DESC"
)
collect(voilationsByTimeBins2015)
#Resultset 2015
#101   18881      0-4             21
#102    3918      0-4             14
#103     697      0-4             20
#244  187924      4-8             69
#245  184213      4-8             14
#246  125938      4-8             20
#134   12861    12-16             14
#135    4459    12-16             18
#136    2474    12-16             38
#307 1294084     8-12             21
#308 1245388     8-12             38
#309  706248     8-12             37
#162   63510    16-20             14
#163   17080    16-20             38
#164    6888    16-20             20
#202   43604    20-24             78
#203   18717    20-24             14
#204    2283    20-24             21
#1    761571     <NA>             36
#2    662208     <NA>              7
#3    609248     <NA>             14


#Year 2016
voilationsByTimeBins2016 <- SparkR::sql(
  "SELECT COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '20-24'
  END AS Time_Bin,
  `Violation Code`
  FROM parking_violations_issued_2016_tbl 
  GROUP BY
    Time_Bin, `Violation Code`
  ORDER BY Time_Bin, COUNT DESC"
)
collect(voilationsByTimeBins2016)
#Resultset 2016
#101   19662      0-4             21
#102    5707      0-4             14
#103     469      0-4             38
#235  176301      4-8             14
#236  163335      4-8             69
#237  114656      4-8             20
#297 1314774     8-12             21
#298 1076854     8-12             38
#299  651231     8-12             37
#135   12876    12-16             14
#136    2840    12-16             18
#137    2725    12-16             38
#164   58716    16-20             14
#165   13924    16-20             38
#166    5207    16-20             20
#195   38755    20-24             78
#196   14709    20-24             14
#197    1903    20-24             21
#1   1253512     <NA>             36
#2    580417     <NA>             46
#3    577649     <NA>             14

#Year 2017
voilationsByTimeBins2017 <- SparkR::sql(
  "SELECT COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '20-24'
  END AS Time_Bin,
  `Violation Code`
  FROM parking_violations_issued_2017_tbl 
  GROUP BY
    Time_Bin, `Violation Code`
  ORDER BY Time_Bin, COUNT DESC"
)
collect(voilationsByTimeBins2017)
#Resultset 2017
#101   22860      0-4             21
#102    3843      0-4             14
#103     546      0-4             38
#221  180105      4-8             14
#222  130723      4-8             69
#223  114804      4-8             21
#277 1298898     8-12             21
#278  999735     8-12             38
#279  567134     8-12             37
#132   14722    12-16             14
#133    3811    12-16             38
#134    2732    12-16             18
#157   60804    16-20             14
#158   12386    16-20             38
#159    4147    16-20             20
#190   32668    20-24             78
#191   13505    20-24             14
#192    2441    20-24             21
#1   1400614     <NA>             36
#2    599904     <NA>             46
#3    592911     <NA>             14

#-----------------------------------------------------------------------------------------------------------------------------------------------
##d)	Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from 	the previous part)

##NA's in the result sets are kept sepearatley as it's not logical to add them in any of the time bin, unless otherwise specified

#Year 2015
top3VoilationsCodesByTimeBins2015 <- SparkR::sql(
  "SELECT `Violation Code`, COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '20-24'
  END AS Time_Bin
  FROM parking_violations_issued_2015_tbl
  WHERE `Violation Code` IN (
    SELECT `Violation Code`
    FROM parking_violations_issued_2015_tbl
    GROUP BY `Violation Code`
    ORDER BY COUNT(*) DESC
    LIMIT 3
  )
  GROUP BY
  `Violation Code`, Time_Bin
  ORDER BY `Violation Code`, COUNT DESC"
)
collect(top3VoilationsCodesByTimeBins2015)
#Resultset 2015
#   Violation Code   COUNT Time_Bin
#1              14  609248     <NA>
#2              14  184213      4-8
#3              14   63510    16-20
#4              14   32160     8-12
#5              14   18717    20-24
#6              14   12861    12-16
#7              14    3918      0-4
#8              21 1294084     8-12
#9              21  103654      4-8
#10             21   82552     <NA>
#11             21   18881      0-4
#12             21    2283    20-24
#13             21     148    16-20
#14             21      12    12-16
#15             38 1245388     8-12
#16             38   43950      4-8
#17             38   17080    16-20
#18             38   13235     <NA>
#19             38    2474    12-16
#20             38    2030    20-24
#21             38     429      0-4

#Year 2016
top3VoilationsCodesByTimeBins2016 <- SparkR::sql(
  "SELECT `Violation Code`, COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '20-24'
  END AS Time_Bin
  FROM parking_violations_issued_2016_tbl
  WHERE `Violation Code` IN (
    SELECT `Violation Code`
    FROM parking_violations_issued_2016_tbl
    GROUP BY `Violation Code`
    ORDER BY COUNT(*) DESC
    LIMIT 3
  )
  GROUP BY
  `Violation Code`, Time_Bin
  ORDER BY `Violation Code`, COUNT DESC"
)
collect(top3VoilationsCodesByTimeBins2016)
#Resultset 2016
#   Violation Code   COUNT Time_Bin
#1              21 1314774     8-12
#2              21  110287      4-8
#3              21   84819     <NA>
#4              21   19662      0-4
#5              21    1903    20-24
#6              21     137    16-20
#7              21       5    12-16
#8              36 1253512     <NA>
#9              38 1076854     8-12
#10             38   35880      4-8
#11             38   13924    16-20
#12             38   12013     <NA>
#13             38    2725    12-16
#14             38    1831    20-24
#15             38     469      0-4

#Year 2017
top3VoilationsCodesByTimeBins2017 <- SparkR::sql(
  "SELECT `Violation Code`, COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '20-24'
  END AS Time_Bin
  FROM parking_violations_issued_2017_tbl
  WHERE `Violation Code` IN (
    SELECT `Violation Code`
    FROM parking_violations_issued_2017_tbl
    GROUP BY `Violation Code`
    ORDER BY COUNT(*) DESC
    LIMIT 3
  )
  GROUP BY
  `Violation Code`, Time_Bin
  ORDER BY `Violation Code`, COUNT DESC"
)
collect(top3VoilationsCodesByTimeBins2017)
#Resultset 2017
#   Violation Code   COUNT Time_Bin
#1              21 1298898     8-12
#2              21  114804      4-8
#3              21   89317     <NA>
#4              21   22860      0-4
#5              21    2441    20-24
#6              21     232    16-20
#7              21      36    12-16
#8              36 1400614     <NA>
#9              38  999735     8-12
#10             38   31931      4-8
#11             38   12386    16-20
#12             38   12341     <NA>
#13             38    3811    12-16
#14             38    1554    20-24
#15             38     546      0-4




#-----------------------------------------------------------------------------------------------------------------------------------------------
#6)))
## Let’s try and find some seasonality in this data
## First, divide the year into some number of seasons, and find frequencies of tickets for each season.
## Then, find the 3 most common violations for each of these season

## Let’s try and find some seasonality in this data
#Year 2016
voilationsBySeason2015 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl
  GROUP BY Season
  ORDER BY COUNT"
)
collect(voilationsBySeason2015)
## Results for 2015:
#Season   COUNT
#1 Winter 2182331
#2   Fall 2718868
#3 Spring 2951328
#4 Summer 3098729

#Year 2016
voilationsBySeason2016 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2016_tbl
  GROUP BY Season
  ORDER BY COUNT"
)
collect(voilationsBySeason2016)
## Results for 2016
#1 Winter 2424488
#2 Summer 2438069
#3 Spring 2790946
#4   Fall 2973396

#Year 2017
voilationsBySeason2017 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2017_tbl
  GROUP BY Season
  ORDER BY COUNT"
)
collect(voilationsBySeason2017)
## Results for 2017
#1 Winter 2485331
#2 Summer 2606208
#3   Fall 2830802
#4 Spring 2880687
#-----------------------------------------------------------------------------------------------------------------------------------------------

## Then, find the 3 most common violations for each of these season
#Year 2015
top3VoilationsCodeForEachSeason2015 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl
  GROUP BY Season, `Violation Code` 
  ORDER BY Season, COUNT DESC"
)
collect(top3VoilationsCodeForEachSeason2015)
#Resultset 2015
#1     Fall             21 351423
#2     Fall             38 326702
#3     Fall             14 232339
#101 Spring             21 425350
#102 Spring             38 327057
#103 Spring             14 243769
#200 Summer             21 471627
#201 Summer             38 363815
#202 Summer             14 255182
#300 Winter             38 307012
#301 Winter             21 253214
#302 Winter             14 193337

#Year 2016
top3VoilationsCodeForEachSeason2016 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2016_tbl
  GROUP BY Season, `Violation Code` 
  ORDER BY Season, COUNT DESC"
)
#Resultset 2016
collect(top3VoilationsCodeForEachSeason2016)
#1     Fall             36 438320
#2     Fall             21 395357
#3     Fall             38 303397
#101 Spring             21 383757
#102 Spring             36 374362
#103 Spring             38 299459
#200 Summer             21 392205
#201 Summer             38 272419
#202 Summer             14 215683
#300 Winter             21 360268
#301 Winter             36 314765
#302 Winter             38 268421

#Year 2017
top3VoilationsCodeForEachSeason2017 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2017_tbl
  GROUP BY Season, `Violation Code` 
  ORDER BY Season, COUNT DESC"
)
collect(top3VoilationsCodeForEachSeason2017)
#Resultset 2017
#1     Fall             36 456046
#2     Fall             21 357479
#3     Fall             38 283828
#100 Spring             21 402807
#101 Spring             36 344834
#102 Spring             38 271192
#200 Summer             21 405961
#201 Summer             38 247561
#202 Summer             36 240396
#298 Winter             21 362341
#299 Winter             36 359338
#300 Winter             38 259723
#-----------------------------------------------------------------------------------------------------------------------------------------------

## The fines collected from all the parking violation constitute a revenue source for the NYC police department. Let’s take an example of estimating that for the 3 most commonly occurring codes.

## Find total occurrences of the 3 most common violation codes

top3VoilationCodes2015 <- SparkR::sql(
  "SELECT `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl
  GROUP BY `Violation Code`
  ORDER BY COUNT(*) DESC
  LIMIT 3"
)
collect(top3VoilationCodes2015)
#1             21 1501614
#2             38 1324586
#3             14  924627

top3VoilationCodes2016 <- SparkR::sql(
  "SELECT `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2016_tbl
  GROUP BY `Violation Code`
  ORDER BY COUNT(*) DESC
  LIMIT 3"
)
collect(top3VoilationCodes2016)
#1             21 1531587
#2             36 1253512
#3             38 1143696

top3VoilationCodes2017 <- SparkR::sql(
  "SELECT `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2017_tbl
  GROUP BY `Violation Code`
  ORDER BY COUNT(*) DESC
  LIMIT 3"
)
collect(top3VoilationCodes2017)
#1             21 1528588
#2             36 1400614
#3             38 1062304


#-----------------------------------------------------------------------------------------------------------------------------------------------


## The fines collected from all the parking violation constitute a revenue source for the NYC police department. Let’s take an example of estimating that for the 3 most commonly occurring codes.

## Find total occurrences of the 3 most common violation codes

top3VoilationCodes2015 <- SparkR::sql(
  "SELECT `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl
  GROUP BY `Violation Code`
  ORDER BY COUNT(*) DESC
  LIMIT 3"
)
collect(top3VoilationCodes2015)
#1             21 1501614
#2             38 1324586
#3             14  924627

top3VoilationCodes2016 <- SparkR::sql(
  "SELECT `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2016_tbl
  GROUP BY `Violation Code`
  ORDER BY COUNT(*) DESC
  LIMIT 3"
)
collect(top3VoilationCodes2016)
#1             21 1531587
#2             36 1253512
#3             38 1143696

top3VoilationCodes2017 <- SparkR::sql(
  "SELECT `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2017_tbl
  GROUP BY `Violation Code`
  ORDER BY COUNT(*) DESC
  LIMIT 3"
)
collect(top3VoilationCodes2017)
#1             21 1528588
#2             36 1400614
#3             38 1062304

#-----------------------------------------------------------------------------------------------
####Total amount collected for all of the fines. Also code which has the highest total collection.

#Year 15
TotalRevenue_15 <- SparkR::sql("select Sum(AvgFine) as RevByViolation from parking_violations_issued_2015_tbl V
                                inner join ViolationFineAmount_tbl VF on V.`Violation Code` = VF.Code")
collect(TotalRevenue_15)
#RevByViolation
#818,170,448

CodeWithHighestCollection_15 <- SparkR::sql("select `Violation Code`, Sum(AvgFine) as RevByViolation from parking_violations_issued_2015_tbl V
                                inner join ViolationFineAmount_tbl VF on V.`Violation Code` = VF.Code  
                                 group by `Violation Code` order by RevByViolation desc LIMIT 1")
collect(CodeWithHighestCollection_15)
#Violation Code RevByViolation
#14      113,673,935

#Year 16
TotalRevenue_16 <- SparkR::sql("select Sum(AvgFine) as RevByViolation from parking_violations_issued_2016_tbl V
                                inner join ViolationFineAmount_tbl VF on V.`Violation Code` = VF.Code")
collect(TotalRevenue_16)
#RevByViolation
#715,966,840

CodeWithHighestCollection_16 <- SparkR::sql("select `Violation Code`, Sum(AvgFine) as RevByViolation from parking_violations_issued_2016_tbl V
                                inner join ViolationFineAmount_tbl VF on V.`Violation Code` = VF.Code  
                                 group by `Violation Code` order by RevByViolation desc LIMIT 1")
collect(CodeWithHighestCollection_16)
#Violation Code RevByViolation
#14      100,695,610

#Year 17
TotalRevenue_17 <- SparkR::sql("select Sum(AvgFine) as RevByViolation from parking_violations_issued_2017_tbl V
                                inner join ViolationFineAmount_tbl VF on V.`Violation Code` = VF.Code")
collect(TotalRevenue_17)
#RevByViolation
#732,576,992

CodeWithHighestCollection_17 <- SparkR::sql("select `Violation Code`, Sum(AvgFine) as RevByViolation from parking_violations_issued_2017_tbl V
                                inner join ViolationFineAmount_tbl VF on V.`Violation Code` = VF.Code  
                                 group by `Violation Code` order by RevByViolation desc LIMIT 1")
collect(CodeWithHighestCollection_17)
#Violation Code RevByViolation
#14      102,752,270

#What can you intuitively infer from these findings
#Top Violation by count in 3 years is with code 21 which is "Street Cleaning: No parking where parking is not allowed by sign, street marking or traffic control device."
#but max revenue is with code 14 which has higher fine for issue "Street Cleaning: No parking where parking is not allowed by sign, street marking or traffic control device."
