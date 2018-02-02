#setting the Working Directory
setwd("D:\\PGDDA\\Course 1\\Investment case study")
getwd()
#Loading all the library packages which we use
library(dplyr)
library(stringr)
library(tidyr)

#Checkpoint 1: Data Cleaning 1

#importing the data from "companies.txt" file into companies dataframe
companies <- read.delim("companies.txt", sep = "\t", header = T, strip.white = F)
#Converting the data of column permalink into lowercase
companies$permalink <- tolower(companies$permalink)

#importing the data from "rounds2.csv" file into rounds2 dataframe
rounds2 <- read.csv("rounds2.csv",  header = T, stringsAsFactors = F)
#converting the data of column company_permalink into lowercase
rounds2$company_permalink <- tolower(rounds2$company_permalink)

#importing the data from "rounds2.csv" file into rounds2 dataframe
mapping <- read.csv('mapping.csv', header = T, stringsAsFactors = F)


#How many unique companies are present in rounds2?
length(unique(rounds2$company_permalink))

#How many unique companies are present in companies?
length(unique(companies$permalink))

#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
# Primary key in company dataframe is "permalink"


#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no:
# Answers is "No". company_permalink will act as foriegn key.


#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
#Name the merged frame master_frame. How many observations are present in master_frame?
#merging the dataframes companies and rounds2 into master_frame dataframe
master_frame <- merge(companies, rounds2, by.x = "permalink", by.y = "company_permalink" )



#Checkpoint 2: Funding Type Analysis

#Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity).
#replace all the blank values by NA

master_frame[master_frame == ""] <- NA
funding <- aggregate(master_frame$raised_amount_usd,by=list(funding_round_type = master_frame$funding_round_type), FUN = mean, na.rm = T)
#colnames(funding[2]) <- "mean_raised_amount_usd"
names(funding)[2] <- "mean_raised_amount_usd"
investment_type <- filter(funding, funding_round_type %in% c("venture", "angel", "seed", "private_equity"), (funding$mean_raised_amount_usd >= 5000000 & funding$mean_raised_amount_usd <= 15000000))
investment_type <- arrange(investment_type, desc(investment_type$mean_raised_amount_usd))
head(investment_type$funding_round_type, 1)

#Checkpoint 3: Country Analysis

#Top nine countries which have received the highest total funding 
new_venture <- filter(master_frame, funding_round_type %in% "venture")
country_wise_total_usd <- aggregate(new_venture$raised_amount_usd, by = list(new_venture$country_code), FUN = sum, na.rm = TRUE)
top9 <- head(arrange(country_wise_total_usd, desc(country_wise_total_usd$x)), 9)


#Checkpoint 4: Sector Analysis 1

#Preaparation of data for Sector Analysis
category <- gather(mapping,category,value,c(2:10))
category <- category[!(category$value == 0), ]
category <- category[,-3]
category <- category[!(category$category_list == ""),]
category <- category[which(!is.na(category$category_list)),]
category$category_list <- tolower(category$category_list)
category$category_list <-gsub('0',"na",category$category_list)
category$category_list[which(category$category_list == "enterprise 2.na")] <- "enterprise 2.0"
clean1 <- master_frame[which(!is.na(master_frame$category_list)),]

clean2 <- separate(clean1, category_list, into = c("primarysector","secondary","x","y","z"), sep="\\|")                   
clean2$primarysector <- tolower(clean2$primarysector)

#Merging of master_frame and category after cleaning
sector1 <- merge(clean2, category, by.x = "primarysector", by.y = "category_list")


#Checkpoint 5: Sector Analysis 2

#Data frames D1, D2 and D3 for each of the three countries 
#containing the observations of funding type FT falling within the 5-15 million USD range.
D1 <- filter(sector1, funding_round_type %in% "venture", country_code %in% c("USA"), raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
D2 <- filter(sector1, funding_round_type %in% "venture", country_code %in% c("GBR"), raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
D3 <- filter(sector1, funding_round_type %in% "venture", country_code %in% c("IND"), raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)

#Total number of investments 
nrow(D1)
nrow(D2)
nrow(D3)


#Total amount of investment in USD
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)


#Top sector (based on count of investments)
usa_category <- group_by(D1, D1$category)
summarise(usa_category, total.count=n(), na.rm=T)

gbr_category <- group_by(D2 ,D2$category)
summarise(gbr_category,total.count=n(),na.rm=T)

ind_category <- group_by(D3, D3$category)
summarise(ind_category,total.count=n(),na.rm=T)



usa_funding <- aggregate(D1$raised_amount_usd, by=list(D1$category), FUN = sum, na.rm = T)
arrange(usa_funding,desc(usa_funding$x))

gbr_funding <- aggregate(D2$raised_amount_usd, by=list(D2$category),FUN = sum, na.rm = T)
arrange(gbr_funding,desc(gbr_funding$x))

ind_funding <- aggregate(D3$raised_amount_usd, by=list(D3$category), FUN = sum, na.rm = T)
arrange(ind_funding,desc(ind_funding$x))

#For the top sector count-wise (point 3), which company received the highest investment
D1$category <- tolower(D1$category)
D1_Others <- filter(D1, D1$category == "others")
D1_others_agg <- aggregate(D1_Others$raised_amount_usd, by = list(D1_Others$name), FUN = sum)
head(arrange(D1_others_agg, desc(D1_others_agg$x)), 1) [1]

D2$category <- tolower(D2$category)
D2_Others <- filter(D2, D2$category == "others")
D2_others_agg <- aggregate(D2_Others$raised_amount_usd, by = list(D2_Others$name), FUN = sum)
head(arrange(D2_others_agg, desc(D2_others_agg$x)), 1) [1]

D3$category <- tolower(D3$category)
D3_Others <- filter(D3, D3$category == "others")
D3_others_agg <- aggregate(D3_Others$raised_amount_usd, by = list(D3_Others$name), FUN = sum)
head(arrange(D3_others_agg, desc(D3_others_agg$x)), 1) [1]

#For the second-best sector count-wise (point 4), which company received the highest investment?
D1_Social <- filter(D1, D1$category == "social..finance..analytics..advertising")
D1_Social_agg <- aggregate(D1_Social$raised_amount_usd, by = list(D1_Social$name), FUN = sum)
head(arrange(D1_Social_agg, desc(D1_Social_agg$x)), 1)[1] 


D2_Social <- filter(D2, D2$category == "social..finance..analytics..advertising")
D2_Social_agg <- aggregate(D2_Social$raised_amount_usd, by = list(D2_Social$name), FUN = sum)
head(arrange(D2_Social_agg, desc(D2_Social_agg$x)), 1) [1]


D3_Social <- filter(D3, D3$category == "social..finance..analytics..advertising")
D3_Social_agg <- aggregate(D3_Social$raised_amount_usd, by = list(D3_Social$name), FUN = sum)
head(arrange(D3_Social_agg, desc(D3_Social_agg$x)), 1) [1]

