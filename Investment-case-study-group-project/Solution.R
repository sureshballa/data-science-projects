library(dplyr)

?read.delim
companies <- read.delim("companies.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
write.csv(companies, "companies.csv")
summary(companies)
nrow(companies)
typeof(companies$city)
View(companies)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
nrow(rounds2)
summary(rounds2)
View(rounds2)

str(companies)
distinct_companies <- distinct(companies, permalink)
distinch_companies_round2 <- distinct(rounds2, company_permalink)
typeof(distinct_companies)
nrow(distinct_companies)
nrow(distinch_companies_round2)

rounds2_company_groups <- group_by(rounds2, company_permalink)
rounds2_company_groups_summary <- summarise(rounds2_company_groups, count = n())
rounds2_multiple_companies <- filter(rounds2_company_groups_summary, count > 1)
sum(rounds2_multiple_companies$count)
nrow(rounds2_multiple_companies)

View(rounds2_multiple_companies)

round2_new_companies <- subset(rounds2, !(tolower(rounds2$company_permalink) %in% tolower(companies$permalink)))
View(round2_new_companies)
nrow(round2_new_companies)
print(round2_new_companies[2,1])

