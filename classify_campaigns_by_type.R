rm(list=ls())
gc()

setwd("~/Documents/Ann/campaign_id")
load("~/Documents/Ann/network/clean.shark.partial")

campaigns_data <- clean.shark.partial[ c("campaign_id","dissident_tactic") ]

# put problematic tactics by hand:
tactic_list <- c("13D Filer - Hostile Item 4",
                 "13D Filer - No Publicly Disclosed Activism",
                 "Withhold Vote for Director\\(s\\)")

# generate tactic_list:
dissident_tactic <- campaigns_data$dissident_tactic
dissident_tactic <- gsub(paste(tactic_list,collapse="|"),"",dissident_tactic)
tactic_new <- strsplit(gsub("([a-z])([A-Z])","\\1___\\2",dissident_tactic),"___")
tactic_list <- append(tactic_list,unique(unlist(tactic_new)))
tactic_list <- tactic_list[complete.cases(tactic_list)]
tactic_list <- sort(tactic_list)
print(tactic_list)


# check remaining tactics:
tactic_remain <- unique(gsub(paste(tactic_list,collapse="|"),"",campaigns_data$dissident_tactic))
print(tactic_remain)

# build logical data.frame:
for ( i in 1:length(tactic_list) ) {
  tactic <- tactic_list[i]
  campaigns_data[[tactic]] <- grepl(tactic,campaigns_data$dissident_tactic)
}

save(campaigns_data,file="campaigns_data")