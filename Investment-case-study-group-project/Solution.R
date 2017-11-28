library(dplyr)
library(tidyr)

## Please make sure to set current working directory to same directory where all the files are present
companies <- read.delim("companies.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")

## Remove encoding and have everything to lower so that we can get accurate result to find unique records
companies$permalink <- iconv(companies$permalink, "UTF-8", "ASCII", sub="")
companies$permalink <- tolower(companies$permalink)

# Use this command to visualize in excel. write.csv(companies, "companies.csv")
summary(companies)
nrow(companies)

## Start of Checkpoint 1: Data Cleaning 1

rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

## Remove encoding and have everything to lower so that we can get accurate result to find unique records
rounds2$company_permalink <- iconv(rounds2$company_permalink, "UTF-8", "ASCII", sub="")
rounds2$company_permalink <- tolower(rounds2$company_permalink)

nrow(rounds2)
summary(rounds2)

#19990 has NA for raised_amount_usd

distinct_companies <- distinct(companies, permalink)
distinch_companies_round2 <- distinct(rounds2, company_permalink)

nrow(distinct_companies)
nrow(distinch_companies_round2)

rounds2_company_groups <- group_by(rounds2, company_permalink)
rounds2_company_groups_summary <- summarise(rounds2_company_groups, count = n())
rounds2_multiple_companies <- filter(rounds2_company_groups_summary, count > 1)
sum(rounds2_multiple_companies$count)
nrow(rounds2_multiple_companies)

round2_new_companies <- subset(rounds2, !(rounds2$company_permalink %in% companies$permalink))
nrow(round2_new_companies)

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

investments_for_post_ipo_debt <- filter(master_frame, funding_round_type == "venture", is_english_speaking == TRUE)
top9_english_speaking_countries_of_investments_for_post_ipo_debt <- group_by(investments_for_post_ipo_debt, country)
top9 <- summarise(top9_english_speaking_countries_of_investments_for_post_ipo_debt, raised_amount_usd = sum(raised_amount_usd, na.rm = T))
arrange(top9, desc(top9$raised_amount_usd))

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

#sapply is taking time, wait for statement to complete execution
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

## Use below command to export all computed data and use it in tableau for plots
## write.csv(master_frame, "master_frame.csv")
