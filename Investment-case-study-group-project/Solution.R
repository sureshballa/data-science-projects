library(dplyr)
library(tidyr)

## Please make sure to set current working directory to same directory where all the files are present
companies <- read.delim("companies.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")

## Remove encoding and have everything to lower so that we can get accurate result to find unique records
companies$permalink <- iconv(companies$permalink, "UTF-8", "ASCII", sub="")
companies$permalink <- tolower(companies$permalink)

## Use this command to visualize in excel. write.csv(companies, "companies.csv")
summary(companies)
nrow(companies)

## Start of Checkpoint 1: Data Cleaning 1

rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

## Remove encoding and have everything to lower so that we can get accurate result to find unique records
rounds2$company_permalink <- iconv(rounds2$company_permalink, "UTF-8", "ASCII", sub="")
rounds2$company_permalink <- tolower(rounds2$company_permalink)

nrow(rounds2)
summary(rounds2)

## 19990 has NA for raised_amount_usd

## unique companies 
distinct_companies <- distinct(companies, permalink)
distinch_companies_round2 <- distinct(rounds2, company_permalink)

## other approach
#library("sqldf")
#sqldf("select Count(distinct company_permalink) as FundedCompanyCount from rounds2;")

## unique companies in rounds2/ Funded 
nrow(distinct_companies)
nrow(distinch_companies_round2)
## other approach
#sqldf("select Count(distinct permalink) as CompanyCount from companies;")

## companies in the rounds2 file which are not present in companies 
round2_new_companies <- subset(rounds2, !(rounds2$company_permalink %in% companies$permalink))
nrow(round2_new_companies)
## other approach
#setdiff(levels(rounds2$company_permalink),levels(companies$permalink))

## build master frame - merge companies & rounds2 
master_frame <- merge(companies, rounds2, by.x = "permalink", by.y  = "company_permalink")
nrow(master_frame)

## End of Checkpoint 1: Data Cleaning 1

## Start of Checkpoint 2: Funding Type Analysis

## TODO: Name the columns appropriately in below groupings
avg_funding <- aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN = mean, na.action=na.pass, na.rm=TRUE)
avg_funding$fund_raised_in_million <- avg_funding$x / 1000000
## Spark Funds wants to choose one of these four investment types for each potential investment they will make. Four investments mentioned are Seed, angel, venture, Private equity
avg_funding_with_constrained_funding_types <- filter(avg_funding, avg_funding$Group.1 == "venture" | avg_funding$Group.1 == "angel" | avg_funding$Group.1 == "seed" | avg_funding$Group.1 == "private_equity")
arrange(avg_funding_with_constrained_funding_types, desc(avg_funding_with_constrained_funding_types$fund_raised_in_million))

## other approach
#sqldf("select funding_round_type, Avg(raised_amount_usd) 
#      from rounds2 where funding_round_type in ('venture', 'angel', 'seed', 'private_equity') 
#      group by funding_round_type;")

## Above query yeilds funding type of venture has most investments. So lets go ahead with venture for next steps

## End of Checkpoint 2: Funding Type Analysis

## Start of Checkpoint 3: Country Analysis

install.packages("countrycode")
library("countrycode")
#countrycode(c('USA', 'DZA', 'BAH'), 'iso3c', 'country.name')
#custom match is being done for few countries for which countrycode doesnt have mappings
custom_match <- c(BAH = 'Bahamas', ROM = "Romania", TAN = "Tanzania")
master_frame$country <- countrycode(master_frame$country_code, "iso3c", "country.name", custom_match = custom_match)

# These below values are from PDF link given
# Note United Kingdom of Great Britain and Northern Ireland is being considered as english speaking country
english_speaking_countries_africa <- c('Botswana', 'Cameroon', 'Ethiopia', 'Eritrea', 'The Gambia', 'Ghana', 'Kenya', 'Lesotho', 'Liberia', 'Malawi', 'Mauritius', 'Namibia', 'Nigeria', 'Rwanda', 'Seychelles', 'Sierra Leone', 'South Africa', 'South Sudan', 'Sudan', 'Swaziland', 'Tanzania', 'Uganda', 'Zambia', 'Zimbabwe')
english_speaking_countries_americas <- c('Antigua and Barbuda', 'The Bahamas', 'Barbados', 'Belize', 'Canada', 'Dominica', 'Grenada', 'Guyana', 'Jamaica', 'Saint Kitts and Nevis', 'Saint Lucia', 'Saint Vincent and the Grenadines', 'Trinidad and Tobago', 'United States', 'United States of America', 'United Kingdom of Great Britain and Northern Ireland')
english_speaking_countries_asia <- c('India', 'Pakistan', 'Philippines', 'Singapore')
english_speaking_countries_australia <- c('Australia', 'Fiji', 'Kiribati', 'Marshall Islands', 'Federated States of Micronesia', 'Nauru', 'New Zealand', 'Palau', 'Papua New Guinea', 'Samoa', 'Solomon Islands', 'Tonga', 'Tuvalu', 'Vanuatu')

all_english_speaking_countries <- c(english_speaking_countries_africa, english_speaking_countries_americas, english_speaking_countries_asia, english_speaking_countries_australia)

master_frame$is_english_speaking <- sapply(master_frame$country, function(colValue) is.element(colValue, all_english_speaking_countries))

## top 9
investments_venture <- filter(master_frame, funding_round_type == "venture")
top_countries_of_investments_group <- group_by(investments_venture, country)
top_countries_of_investments <- summarise(top_countries_of_investments_group, raised_amount_usd = sum(raised_amount_usd, na.rm = T))
top9 = head(arrange(top_countries_of_investments, desc(top_countries_of_investments$raised_amount_usd)), 9)

## top english speaking countries by investment within the top 9 set
top9$country[which(is.element(top9$country, all_english_speaking_countries))]

## other approach
#top9 <- sqldf("select country_code, sum(raised_amount_usd)
#           from master_frame
#           where funding_round_type='venture'
#           group by country_code
#           order by 2 desc
#           LIMIT 9;")

## Above query yeilds top 3 countries as United States of America, United Kingdom of Great Britain and Northern Ireland, India
## Assumption here is - "United Kingdom of Great Britain and Northern Ireland" is english speaking country - meaning english as official language

## End of Checkpoint 3: Country Analysis

## Start of Checkpoint 4: Sector Analysis 1

sector_mappings <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE, check.names=FALSE)
sector_mappings_long_format <- gather(sector_mappings, sector, sector_val, 2:10)
sector_mappings_long_format <- sector_mappings_long_format[!(sector_mappings_long_format$sector_val == 0),]
sector_mappings_long_format <- sector_mappings_long_format[, -3]

## Clean up sector mappings. 
## A0lytics, Big Data A0lytics, Business A0lytics are few examples
## There is pattern to be observed here - na is replaced with 0,
## so fix by replacing 0 with na

install.packages("stringr")
library("stringr")

sector_mappings_long_format$category_list <- str_replace_all(sector_mappings_long_format$category_list, "0", "na")

master_frame$primary_sector <- sapply(master_frame$category_list, function(category_list) strsplit(category_list, "\\|")[[1]][1])

getMainSector <- function(primary_sector_value) {
  matchRecord <- filter(sector_mappings_long_format, tolower(sector_mappings_long_format$category_list) == tolower(primary_sector_value))
  matchRecord$sector[1]
}

#sapply is taking time, wait for statement to complete execution. TODO: Need to optimise this, will work on it
master_frame$main_sector <- sapply(master_frame$primary_sector, getMainSector)

## End of Checkpoint 4: Sector Analysis 1

## Start of Checkpoint 5: Sector Analysis 2

## Based in results from Checkpoint 3: Country Analysis, top 3 countries are
## United States of America, United Kingdom of Great Britain and Northern Ireland, India

sector_groups_d1 <- group_by(filter(master_frame, master_frame$funding_round_type == "venture", master_frame$country == "United States of America"), main_sector)
sector_groups_d1_summary <- summarise(sector_groups_d1, count = n(), total_amount_invested = sum(raised_amount_usd, na.rm = T))
D1 <- arrange(sector_groups_d1_summary, desc(total_amount_invested))

sector_groups_d2 <- group_by(filter(master_frame, master_frame$funding_round_type == "venture", master_frame$country == "United Kingdom of Great Britain and Northern Ireland"), main_sector)
sector_groups_d2_summary <- summarise(sector_groups_d2, count = n(), total_amount_invested = sum(raised_amount_usd, na.rm = T))
D2 <- arrange(sector_groups_d2_summary, desc(total_amount_invested))

sector_groups_d3 <- group_by(filter(master_frame, master_frame$funding_round_type == "venture", master_frame$country == "India"), main_sector)
sector_groups_d3_summary <- summarise(sector_groups_d3, count = n(), total_amount_invested = sum(raised_amount_usd, na.rm = T))
D3 <- arrange(sector_groups_d3_summary, desc(total_amount_invested))

## End of Checkpoint 5: Sector Analysis 2

## Answers for Table 5.1 : Sector-wise Investment Analysis
## Requirement: all the observations refer to investments of the type FT within 5-15 M USD range. 
## So we will fiter FT = venture and not really by range
## Will save D1, D2 and D3 as csv and find answers for Table 5.1 questions
## write.csv(D1, "D1.csv")
## write.csv(D2, "D2.csv")
## write.csv(D3, "D3.csv")

## Use below command to export all computed data and use it in tableau for plots
## write.csv(master_frame, "master_frame.csv")
