
# This code performs separating the fund names string and matching of the fund names to its CIKs

rm(list=ls())

setwd("~/Networks/Analysis")
library(dplyr)
library(plyr)
load("sharkwatch")
load("file.list.13fg")

list.shark <-  unique(sharkwatch$dissident_group) # Create the list of activist names. activist == dissident
fund.data <- unique( file.list.13fg[,c(1,3)] )
fund.data$company_name.long <-  toupper(  gsub(",|\\.|'|\\?|/|-", "", fund.data$company_name) )

# Test how well it is searched

# Delete buzzwords 

test<-gsub("Inc|LLC|Corp|Corporation|Ltd|Asset|Management|Partners|L.L.C.|Investments|
Investors|Investment|Investors|L.P.|Holdings|Limited|LC|Associates|International
|LP|Advisors|Advisers|Advisory|Group|Solutions|&|Co.|Technologies|\\(|\\)", "", list.shark)  # target specific strings
test.clean <- gsub("\\s*|^\\s+|\\s+$|,|\\.", "", test)        # target all characters after comma
short <- toupper(test.clean)
long <- toupper(  gsub(",|\\.|'|\\?|/|-", "", list.shark) )

shark.long.short <- as.data.frame( cbind(list.shark, short, long) )

matched.funds <- data.frame()

for(i in 1:length(short)){

to.find <- shark.long.short$short[i]

row.number <- pmatch(to.find, fund.data$company_name)

if ( ! is.na(row.number) ){
out <- cbind( i, fund.data[row.number,], shark.long.short[i,])
matched.funds <- rbind(matched.funds,out)
}

}

# Do a "long' match

matched.funds.long <- data.frame()

for(i in 1:length(short)){
  
  to.find <- shark.long.short$long[i]
  
  row.number <- pmatch(to.find, fund.data$company_name.long)
  
  if ( ! is.na(row.number) ){
    out <- cbind( i, fund.data[row.number,], shark.long.short[i,])
    matched.funds.long <- rbind(matched.funds.long,out)
  }
  
}

# Get the non-matched list

is <- matched.funds.long$i
all <- c(  1:length(long) )
nis <- setdiff(all,is)

not.matched.long <- shark.long.short %>% "["(.,nis,)

# delete the matched funds from the whole fundslist

funds.not.matched <- filter(fund.data, !cik %in% matched.funds.long$cik)

not.matched.long <- read.csv("not.matched.long.csv")

# Bind them together again  (csv file is the file with delimiters)
matched.long.list <- matched.funds.long[c("list.shark", "long")]
not.matched.ist <- not.matched.long[c("list.shark", "long")]
                                    
delimeters.list <- unique( bind_rows(matched.long.list, not.matched.ist) )    

delim.shark <- merge(sharkwatch, delimeters.list, by.x="dissident_group", by.y="list.shark", all.x=TRUE)


# Break the database using delimeters
cleaned.shark <- data.frame()

for ( i in 1:nrow(delim.shark) ){
  
  print(i)
  
  candidate.string <-  delim.shark$long[i]

  s.vector =  unlist( strsplit( candidate.string, ";") )
 
  # for each element of the vector, copy a string: 
  string <- delim.shark[i , !(names(delim.shark) %in% c("long"))]
 
  cleaned <- data.frame()
 
 for (company in 1:length(s.vector) ){
   
   activist.name = s.vector[company]
   out <- cbind(activist.name, string)
   cleaned <- rbind(cleaned, out)
 }
 
 cleaned.shark <- rbind (cleaned.shark, cleaned) 
 
}
 

save(cleaned.shark, file="cleaned.shark")
# a = cleaned.shark[c("dissident_group", "activist.name", "company_name", "outcome")]

# Now, repeat the binding procedure

list.shark <-  unique(cleaned.shark[c("activist.name","dissident_group") ]) 
fund.data <- unique( file.list.13fg[,c(1,3)] )
fund.data$company_name.long <-  toupper(  gsub(",|\\.|'|\\?|/|-", "", fund.data$company_name) )

# Do a "long"  match

matched.clean.long <- data.frame()

for(i in 1:nrow(list.shark)){
  
  to.find <- list.shark$activist.name[i]
  
  row.number <- pmatch(to.find, fund.data$company_name.long)
  
  if ( ! is.na(row.number) ){
    out <- cbind( i, fund.data[row.number,], list.shark[i,])
    matched.clean.long <- bind_rows(matched.clean.long,out)
  }
  
}

matched.clean.long <- matched.clean.long[c("activist.name","cik")]
# THIS IS VERY IMPORTANT STEP!
matched.clean.long = unique(matched.clean.long)

clean.shark.partial <- merge(cleaned.shark, matched.clean.long, by="activist.name", all=TRUE)

save(clean.shark.partial, file="clean.shark.partial")


load("clean.shark.partial")
# a <- clean.shark.partial[c("activist.name","cik","announce_date", "date_original_13d_filed", 
                           # "dissident_board_seats_wongranted_date", "end_date", "meeting_date",
                           # "proxy_fight_announce_date", "outcome", "campaign_status")]

second_part=read.csv("shark_matched_cut_135_2.csv")
second_part = unique(second_part)
second_part = second_part[c("names_tomatch", "names_split_full", "cik", "ind.workplace")]
names(second_part) = c("dissident_group", "activist.name", "cik", "cik_workplace")

shark_bind <- merge(second_part, sharkwatch, by ="dissident_group")
shark_bind=unique(shark_bind)

clean.shark.final = rbind.fill(clean.shark.partial, shark_bind)
clean.shark.final=unique(clean.shark.final)


save(clean.shark.final, file="clean.shark.final")


load("clean.shark.final")
act_age <- read.csv("activists_age_checked.csv")
act_age = act_age[c("activist.name", "cik", "fund_founding_year")]
names(act_age) <- c("activist.name", "cik_checked", "fund_founding_year")

clean.shark.final_age <- merge( clean.shark.final, act_age, by= "activist.name", all.x=T )
clean.shark.final_age <- unique( clean.shark.final_age )
camp_year <- substr( clean.shark.final_age$announce_date, 1,4)
camp_year <- as.numeric( camp_year )
# Compute age of each fund:


clean.shark.final_age$age_activist <- camp_year - clean.shark.final_age$fund_founding_year

# Now, change ciks of the funds in the file for checked_cik

cik_list <- act_age$cik_checked

subset <- clean.shark.final_age[ which( clean.shark.final_age$cik %in% cik_list), ]
subset$cik = subset$cik_checked
'%!in%' <- function(x,y)!('%in%'(x,y))
non_subset <- clean.shark.final_age[ which( clean.shark.final_age$cik %!in% cik_list), ]

clean.shark.final_age <- rbind( subset, non_subset)

clean.shark.final = clean.shark.final_age

activist_names = (clean.shark.final$activist.name)


activist_appearance_number = function( name ){ 
  
  number= nrow( clean.shark.final[which(clean.shark.final$activist.name == name),] )
  return(number)
  
}

numbers = do.call( c, lapply(activist_names, activist_appearance_number) )

clean.shark.final$activist_appearance_number = numbers

save(clean.shark.final, file="clean.shark.final_age")

load("short.data.compust_2000_2015")
survived_campaigns = short.data.compust$campaign.id

load("clean.shark.final_age")

survived_shark = unique( clean.shark.final[which(clean.shark.final$campaign_id %in% survived_campaigns),])
survived_shark = unique( survived_shark[, c("campaign_id", "activist.name")])

survived_activist_appearance_number = data.frame()
for(campaign.id in survived_campaigns){
  
  name = unique( survived_shark$activist.name[which(survived_shark$campaign_id == campaign.id)]) 
  surv_act_appearance_number= nrow( survived_shark[which(survived_shark$activist.name == name),] )
  out = c(campaign.id,  surv_act_appearance_number)
  survived_activist_appearance_number = rbind(survived_activist_appearance_number, out)
  
}

names(survived_activist_appearance_number) = c("campaign.id", "surv_act_appearance_number")
survived_activist_appearance_number = unique(survived_activist_appearance_number)
save(survived_activist_appearance_number, file="survived_activist_appearance_number")
