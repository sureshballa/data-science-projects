## Instruction to reviewer: Please make sure to set correct path for s3 based files
bucket_path <- "s3://data-science-big-data-analytics-suresh/nyc-parking-case-study/";
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
#-----------------------------------------------------------------------------------------
#2) Find out how many unique states the cars which got parking tickets came from.
nrow(distinct(select(parking_violations_issued_2015, 'Registration State')))
## Unique States in 2015: 69
nrow(distinct(select(parking_violations_issued_2016, 'Registration State')))
## Unique States in 2016: 68
nrow(distinct(select(parking_violations_issued_2017, 'Registration State')))
## Unique States in 2017: 67
#------------------------------------------------------------------------------------------
#3) Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are.
nrow(filter(parking_violations_issued_2015, isNull(parking_violations_issued_2015$`Street Name`) | parking_violations_issued_2015$`Street Name` == '' | isNull(parking_violations_issued_2015$`House Number`) | parking_violations_issued_2015$`House Number` == ''))
## Parking tickets without address 2015: 1992401
nrow(filter(parking_violations_issued_2016, isNull(parking_violations_issued_2016$`Street Name`) | parking_violations_issued_2016$`Street Name` == '' | isNull(parking_violations_issued_2016$`House Number`) | parking_violations_issued_2016$`House Number` == ''))
## Parking tickets without address 2016: 2035232
nrow(filter(parking_violations_issued_2017, isNull(parking_violations_issued_2017$`Street Name`) | parking_violations_issued_2017$`Street Name` == '' | isNull(parking_violations_issued_2017$`House Number`) | parking_violations_issued_2017$`House Number` == ''))
## Parking tickets without address 2016: 2035232
################################ Examine the data ####################################

# For using SQL, you need to create a temporary view
createOrReplaceTempView(parking_violations_issued_2015, "parking_violations_issued_2015_tbl")
createOrReplaceTempView(parking_violations_issued_2016, "parking_violations_issued_2016_tbl")
createOrReplaceTempView(parking_violations_issued_2017, "parking_violations_issued_2017_tbl")

################################ Aggregation tasks ##########################################
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
#1               21  1630912
#2               38  1418627
#3               14   988469
#4               36   839197
#5               37   795918

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

##TODO sample queries, to be removed
voilationsByCodeYear <- SparkR::sql("
SELECT Year, `Violation Code`, NoOfViolation FROM (
    SELECT '2015' AS Year, `Violation Code`, COUNT(*) AS NoOfViolation
    FROM parking_violations_issued_2015_tbl 
    GROUP BY `Violation Code` 
    UNION ALL
    SELECT '2016' AS Year, `Violation Code`, COUNT(*) AS NoOfViolation
    FROM parking_violations_issued_2016_tbl 
    GROUP BY `Violation Code`
    UNION ALL
    SELECT '2017' AS Year, `Violation Code`, COUNT(*) AS NoOfViolation
    FROM parking_violations_issued_2017_tbl 
    GROUP BY `Violation Code`
) AS tbl ORDER BY NoOfViolation DESC LIMIT 5")
collect(voilationsByCodeYear)
#1 2015             21       1630912
#2 2016             21       1531587
#3 2015             38       1418627
#4 2017             21       1383057
#5 2016             36       1253512

voilationsByCode <- SparkR::sql("
SELECT `Violation Code`, SUM(NoOfViolation) AS NoOfViolation FROM (
    SELECT `Violation Code`, COUNT(*) AS NoOfViolation
    FROM parking_violations_issued_2015_tbl 
    GROUP BY `Violation Code` 
    UNION ALL
    SELECT `Violation Code`, COUNT(*) AS NoOfViolation
    FROM parking_violations_issued_2016_tbl 
    GROUP BY `Violation Code`
    UNION ALL
    SELECT `Violation Code`, COUNT(*) AS NoOfViolation
    FROM parking_violations_issued_2017_tbl 
    GROUP BY `Violation Code`
) AS tbl 
GROUP BY `Violation Code`
ORDER BY NoOfViolation DESC")
collect(voilationsByCode)
##TODO sample queries to be removed

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
#1              SUBN  3729346
#2              4DSD  3340014
#3               VAN  1709091
#4              DELV   892781
#5               SDN   524596

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
1              SUBN  3719802
2              4DSD  3082020
3               VAN  1411970
4              DELV   687330
5               SDN   438191



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
#1         FORD  1521874
#2        TOYOT  1217087
#3        HONDA  1102614
#4        NISSA   908783
#5        CHEVR   897845

##Year 2016
voilationsByMake2016 <- SparkR::sql(
  "SELECT `Vehicle Make`, COUNT(*) 
  FROM parking_violations_issued_2016_tbl 
  GROUP BY `Vehicle Make` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByMake2016)
#Violation By Vehicle Make - 2016
1         FORD  1324774
2        TOYOT  1154790
3        HONDA  1014074
4        NISSA   834833
5        CHEVR   759663

##Year 2017
voilationsByMake2017 <- SparkR::sql(
  "SELECT `Vehicle Make`, COUNT(*) 
  FROM parking_violations_issued_2017_tbl 
  GROUP BY `Vehicle Make` 
  ORDER BY COUNT(*) DESC 
  LIMIT 5")
collect(voilationsByMake2017)
#Violation By Vehicle Make - 2017
1         FORD  1280958
2        TOYOT  1211451
3        HONDA  1079238
4        NISSA   918590
5        CHEVR   714655

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
1                  0  1799170
2                 19   598351
3                 18   427510
4                 14   409064
5                  1   329009

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
#1                 0  2037745
#2                19   579998
#3                18   417329
#4                14   392922
#5                 1   318778

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
#Resultset 2015

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
#Resultset 2016

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
#Resultset 2017

#-----------------------------------------------------------------------------------------------------------------------------------------------

## You’d want to find out the properties of parking violations across different times of the day:
## The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
## Find a way to deal with missing values, if any.
## Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the 3 most commonly occurring violations
## Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)

## Lets check missing values
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

##TODO examine for 2016 and 2017 as well

#-----------------------------------------------------------------------------------------------------------------------------------------------

##TODO: What should we do to impute missing values

#-----------------------------------------------------------------------------------------------------------------------------------------------

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

##TODO examine for 2016 and 2017 as well

#-----------------------------------------------------------------------------------------------------------------------------------------------

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
  END AS Time_Bin,
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

## TODO examine for 2016 and 2017 as well

#-----------------------------------------------------------------------------------------------------------------------------------------------

## Let’s try and find some seasonality in this data
## First, divide the year into some number of seasons, and find frequencies of tickets for each season.
## Then, find the 3 most common violations for each of these season

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
## 1   Fall 2794936
## 2 Winter 2899143
## 3 Spring 2956983
## 4 Summer 3158171

## TODO examine for 2016 and 2017 as well
#-----------------------------------------------------------------------------------------------------------------------------------------------

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

## TODO examine for 2016 and 2017 as well
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

## 1             21 1630912
## 2             38 1418627
## 3             14  988469

## TODO examine for 2016 and 2017 as well
#-----------------------------------------------------------------------------------------------------------------------------------------------

## TODO: Please refer the excel file ParkingTicketsCosts where avg is computed and complete this query
revenueByVoilationCodes <- SparkR::sql(
  "SELECT `Violation Code`,
  SUM(
  CASE
    WHEN (CAST(`Violation Code` AS INT) >= 1 AND CAST(`Violation Code` AS INT) <= 5)
      OR (CAST(`Violation Code` AS INT) >= 8 AND CAST(`Violation Code` AS INT) <= 11)
      OR (CAST(`Violation Code` AS INT) >= 13 AND CAST(`Violation Code` AS INT) <= 15)
      OR (CAST(`Violation Code` AS INT) >= 18 AND CAST(`Violation Code` AS INT) <= 19)
      OR (CAST(`Violation Code` AS INT) >= 25 AND CAST(`Violation Code` AS INT) <= 26)
      OR (CAST(`Violation Code` AS INT) >= 29 AND CAST(`Violation Code` AS INT) <= 31)
      OR CAST(`Violation Code` AS INT) = 40 OR CAST(`Violation Code` AS INT) = 59 OR CAST(`Violation Code` AS INT) = 79 
      OR (CAST(`Violation Code` AS INT) >= 45 AND CAST(`Violation Code` AS INT) <= 48)
      OR (CAST(`Violation Code` AS INT) >= 50 AND CAST(`Violation Code` AS INT) <= 56)
      THEN 515
    WHEN CAST(`Violation Code` AS INT) = 7
      THEN 50
  END
  ) AS Revenue
  FROM parking_violations_issued_2015_tbl
  GROUP BY
  `Violation Code`
  ORDER BY `Violation Code`, Revenue DESC"
)

collect(revenueByVoilationCodes)
