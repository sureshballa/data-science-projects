library(dplyr)

companies <- read.delim("companies.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")

## Remove encoding and have everything to lower so that we can get accurate result to find unique records
companies$permalink <- iconv(companies$permalink, "UTF-8", "ASCII", sub="")
companies$permalink <- tolower(companies$permalink)

# write.csv(companies, "companies.csv")
summary(companies)
nrow(companies)

rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

## Remove encoding and have everything to lower so that we can get accurate result to find unique records
rounds2$company_permalink <- iconv(rounds2$company_permalink, "UTF-8", "ASCII", sub="")
rounds2$company_permalink <- tolower(rounds2$company_permalink)

nrow(rounds2)
summary(rounds2)


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

master_frame <- merge(companies, rounds2, by.x = "permalink", by.y = "company_permalink")
nrow(master_frame)
