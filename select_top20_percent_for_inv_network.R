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

# save(top20_percent_07, file="top20_percent_07")
save(top20_percent_07, file="top20_percent_07_new")

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

# save(top20_percent_13, file="top20_percent_13")
save(top20_percent_13, file="top20_percent_13_new")

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

# save(top20_percent_14, file="top20_percent_14")
save(top20_percent_14, file="top20_percent_14_new")

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

# save(top20_percent_15, file="top20_percent_15")
save(top20_percent_15, file="top20_percent_15_new")

top20_percent = c(top20_percent_07, top20_percent_13, top20_percent_14, top20_percent_15)
top20_percent = unique (top20_percent)

# save(top20_percent, file="top20_percent")
save(top20_percent, file="top20_percent_new")


#-------------------Do top 10


setwd("~/Networks/Analysis")
temp.space <- new.env()
cusip_all <- get(load("dt_07", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top10_percent_07 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top10 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(90)/100) )  ]
  
  top10_percent_07 <- c(top10_percent_07, as.numeric(top10) )
}

top10_percent_07 = unique(top10_percent_07)

# save(top10_percent_07, file="top10_percent_07")
save(top10_percent_07, file="top10_percent_07_new")


temp.space <- new.env()
cusip_all <- get(load("dt_13", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top10_percent_13 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top10 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(90)/100) )  ]
  
  top10_percent_13 <- c(top10_percent_13, as.numeric(top10) )
}

top10_percent_13 = unique(top10_percent_13)

# save(top10_percent_13, file="top10_percent_13")
save(top10_percent_13, file="top10_percent_13_new")

temp.space <- new.env()
cusip_all <- get(load("dt_14", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top10_percent_14 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top10 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(90)/100) )  ]
  
  top10_percent_14 <- c(top10_percent_14, as.numeric(top10) )
}

top10_percent_14 = unique(top10_percent_14)

# save(top10_percent_14, file="top10_percent_14")
save(top10_percent_14, file="top10_percent_14_new")

temp.space <- new.env()
cusip_all = get(load("cusip_ok.short", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$period) )

top10_percent_15 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$period == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top10 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(90)/100) )  ]
  
  top10_percent_15 <- c(top10_percent_15, as.numeric(top10) )
}

top10_percent_15 = unique(top10_percent_15)

save(top10_percent_15, file="top10_percent_15")
save(top10_percent_15, file="top10_percent_15_new")

top10_percent = c(top10_percent_07, top10_percent_13, top10_percent_14, top10_percent_15)
top10_percent = unique (top10_percent)

# save(top10_percent, file="top10_percent")
save(top10_percent, file="top10_percent_new")

# Create an ``old'' file



#-----------Top 5\%





top5_percent_07 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top5 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(95)/100) )  ]
  
  top5_percent_07 <- c(top5_percent_07, as.numeric(top5) )
}

top5_percent_07 = unique(top5_percent_07)

# save(top5_percent_07, file="top5_percent_07")
save(top5_percent_07, file="top5_percent_07_new")


temp.space <- new.env()
cusip_all <- get(load("dt_13", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top5_percent_13 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top5 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(95)/100) )  ]
  
  top5_percent_13 <- c(top5_percent_13, as.numeric(top5) )
}

top5_percent_13 = unique(top5_percent_13)

# save(top5_percent_13, file="top5_percent_13")
save(top5_percent_13, file="top5_percent_13_new")

temp.space <- new.env()
cusip_all <- get(load("dt_14", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$date) )

top5_percent_14 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$date == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top5 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(95)/100) )  ]
  
  top5_percent_14 <- c(top5_percent_14, as.numeric(top5) )
}

top5_percent_14 = unique(top5_percent_14)

# save(top5_percent_14, file="top5_percent_14")
save(top5_percent_14, file="top5_percent_14_new")

temp.space <- new.env()
cusip_all = get(load("cusip_ok.short", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???

dates.list <- sort( unique(cusip_all$period) )

top5_percent_15 <- vector()

for ( i in 1:length(dates.list) ){
  
  date = dates.list[i]
  print(date)
  
  csub <-cusip_all[which(cusip_all$period == date ),]
  csub = csub[c("total.value", "cik")]
  csub = unique(csub)
  top5 = csub$cik[   which( csub$total.value > quantile(csub$total.value,  probs = c(95)/100) )  ]
  
  top5_percent_15 <- c(top5_percent_15, as.numeric(top5) )
}

top5_percent_15 = unique(top5_percent_15)

# save(top5_percent_15, file="top5_percent_15")
save(top5_percent_15, file="top5_percent_15_new")

top5_percent = c(top5_percent_07, top5_percent_13, top5_percent_14, top5_percent_15)
top5_percent = unique (top5_percent)

# save(top5_percent, file="top5_percent")
save(top5_percent, file="top5_percent_new")


# Create 'old' files



# Create an old file:

load("top20_percent_07")
load("top20_percent_13")
load("top20_percent_14")
load("top20_percent_15")
top20_percent = c(top20_percent_07, top20_percent_13, top20_percent_14, top20_percent_15)
top20_percent = unique (top20_percent)

save(top20_percent, file="top20_percent_old")

# Create an old file:

load("top10_percent_07")
load("top10_percent_13")
load("top10_percent_14")
load("top10_percent_15")
top10_percent = c(top10_percent_07, top10_percent_13, top10_percent_14, top10_percent_15)
top10_percent = unique (top10_percent)

save(top10_percent, file="top10_percent_old")

# Create an old file:

load("top5_percent_07")
load("top5_percent_13")
load("top5_percent_14")
load("top5_percent_15")
top5_percent = c(top5_percent_07, top5_percent_13, top5_percent_14, top5_percent_15)
top5_percent = unique (top5_percent)

save(top5_percent, file="top5_percent_old")

