# THis file takes Gantchev's data and merges is to the sharkwatch data. The output fule is called ganch_shark_merged and it contains 909 obs
# or 630 campaigns


rm(list=ls())
library(lubridate)
library(dplyr)
setwd("~/Networks/Analysis")

load("clean.shark.final")
# load("sharkwatch")

short_shark = clean.shark.final[c("company_name", "campaign_id", "outcome", "synopsis_text",
                                    "announce_date", "dissident_group","end_date", "cusip_9_digit")]

short_shark = short_shark[(short_shark$announce_date > "1999-12-31" & short_shark$announce_date < "2008-01-01"),]

short_shark$cusip6 <- substr(short_shark$cusip_9_digit, 1,6)
short_shark$year = year(short_shark$announce_date)

# Load Gantchev's data
gantch <- read.csv("HFActivism_2000_2007_anna.csv")

gantch$cusip6 <- substr(gantch$cusip, 1,6)
gantch$start_event_date = as.Date( gantch$start_event_date, "%m/%d/%Y" )
gantch$year <- year(gantch$start_event_date)

test <- merge(gantch, short_shark, by=c("cusip6", "year") )
test = unique(test)

write.csv(test, file="gantch_shor_shark_1.csv")

# Find the campaigns that were not merged above and check why 

t_ganch = test[  ,which( names(test) %in% names(gantch) )  ]

not_matched_ganch = anti_join(gantch, t_ganch, by=c("cusip6", "year"))
not_matched_ganch = unique(not_matched_ganch)

write.csv(not_matched_ganch, file="not_matched_ganch.csv")
write.csv(short_shark, file="short_shark.csv")

# I classified the data and put it into gantch_shor_shark_1_classified.csv

# Load manually checked and classified files

ganch_checked = read.csv("gantch_shor_shark_1_classified_campaign_id.csv")
ganch_checked = unique(ganch_checked)

m =merge(ganch_checked, clean.shark.final, by=c("campaign_id") )
m=unique(m)

add_to_ganch = read.csv("not_matched_ganch_campaign_id.csv")
add_to_ganch = add_to_ganch[ which(!is.na(add_to_ganch$campaign_id) ) ,]
list = unique(add_to_ganch$campaign_id)

add_to_ganch = merge(add_to_ganch, clean.shark.final, by="campaign_id" )
add_to_ganch=unique(add_to_ganch)

ganch_shark_merged = rbind(m,add_to_ganch)
ganch_shark_merged = unique(ganch_shark_merged)

# head(ganch_shark_merged$cik)
# subsample= ganch_shark_merged[c("target", "activist","activist.name","dissident_group", "activist_cik", "target_cik", "cik")]

save(ganch_shark_merged, file="ganch_shark_merged")

# Create a file of campaigns that would need to be classified in the future
classify.list = setdiff(short_shark$campaign_id, ganch_shark_merged$campaign_id)
classify.list = unique( classify.list )

to_clas = short_shark[which(short_shark$campaign_id %in% classify.list),]
to_clas = unique( to_clas )

write.csv(to_clas, file="to_classify_campaigns_2000_2007.csv")
