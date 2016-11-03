rm(list=ls())
gc()

library(data.table)

# First, create a list of funds who are in the top-20% of reporters in each of the reporting periods

setwd("~/Networks/Analysis")
temp.space <- new.env()
cusip_all <- get(load("dt_07", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top20_percent_07 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top20 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(80)/100) )  ]
  
  top20_percent_07 <- c(top20_percent_07, as.numeric(top20) )
}

top20_percent_07 = unique(top20_percent_07)

save(top20_percent_07, file="top20_percent_07")


temp.space <- new.env()
cusip_all <- get(load("dt_13", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top20_percent_13 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top20 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(80)/100) )  ]
  
  top20_percent_13 <- c(top20_percent_13, as.numeric(top20) )
}

top20_percent_13 = unique(top20_percent_13)

save(top20_percent_13, file="top20_percent_13")

temp.space <- new.env()
cusip_all <- get(load("dt_14", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top20_percent_14 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top20 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(80)/100) )  ]
  
  top20_percent_14 <- c(top20_percent_14, as.numeric(top20) )
}

top20_percent_14 = unique(top20_percent_14)

save(top20_percent_14, file="top20_percent_14")

temp.space <- new.env()
cusip_all = get(load("cusip_ok.short", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$period) )

top20_percent_15 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$period == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top20 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(80)/100) )  ]
  
  top20_percent_15 <- c(top20_percent_15, as.numeric(top20) )
}

top20_percent_15 = unique(top20_percent_15)

save(top20_percent_15, file="top20_percent_15")

top20_percent = c(top20_percent_07, top20_percent_13, top20_percent_14, top20_percent_15)
top20_percent = unique (top20_percent)

save(top20_percent, file="top20_percent")

