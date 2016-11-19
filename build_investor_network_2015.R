rm(list=ls())
gc()

library(data.table)

load("top20_percent")
top20_percent = top20_percent[which(!is.na(top20_percent))]

setwd("~/Networks/Analysis")
temp.space <- new.env()
cusip_all <- get(load("cusip_ok.short", temp.space), temp.space)
rm(temp.space)


cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???
cusip_all$period <- as.Date(cusip_all$period, "%m-%d-%Y")


compustat_all <- read.csv("compustat_q_2015.csv")
compustat=compustat_all
compustat$cusip6 <- substr(compustat$cusip,1,6)
compustat$period = as.Date(as.character(compustat$datadate),"%Y%m%d")
# # creation dates:
# compustat_firstdate <- aggregate( compustat[c("datadate")], compustat[c("cusip6")], FUN=min )
# compustat_ages$date <- as.Date(as.character(compustat_ages$datadate),"%Y%m%d")

for (i in 1:nrow(compustat)) {
  if(!is.na(compustat$mkvalt[i])) {
    compustat$market.value.mln[i] <- compustat$mkvaltq[i]
  } else {
    compustat$market.value.mln[i] <- compustat$cshoq[i]*compustat$prccq[i]
  }
}

load("clean.shark.final_age")
full_activist_list <- unique(clean.shark.final$cik)
full_activist_list <- full_activist_list [which(!is.na(full_activist_list))]

quarters <- c("03-31", "06-30", "09-30", "12-31")
q_to_save <- c("1q", "2q", "3q", "4q")
beg_periods <-  c("02-28", "05-31", "08-31", "11-30")
beg_period_leaps <- c("02-29", "05-31", "08-31", "11-30")
end_periods <- c("04-30", "07-31", "10-31", "01-31")
corr <- data.frame(quarters,q_to_save, beg_periods, beg_period_leaps, end_periods)

leap_years <- c("2000", "2004", "2008", "2012", "2016")

network_dates <- sort( unique( cusip_all$period )  )
'%!in%' <- function(x,y)!('%in%'(x,y))

compustat <- unique( compustat[c("cusip6","cusip", "datadate", "market.value.mln", "period")] ) #???
compustat <- compustat[complete.cases(compustat),] #???


for ( nw  in 1:length(network_dates) ){
  
  nw_date <- network_dates[nw]
  print(nw_date)
  # SET CURRENT PERIOD!!!!! (the period of network formation )
  current = as.Date( nw_date, "%Y-%m-%d")
  
  quarter <- substr(nw_date, 6, 10)
  year <- substr(nw_date, 1, 4)
  
  if (year %in% leap_years){beg_p_q <- corr$beg_period_leaps[which(quarters == quarter)]}else{
    beg_p_q <- corr$beg_periods[which(quarters == quarter)]
  }
  
  end_p_q <- corr$end_periods[which(quarters == quarter)]
  
  beg_date <- paste0(year,"-", beg_p_q)
  if( end_p_q == "01-31" ){
    year_end = as.numeric(year)+1
    end_date <- paste0(year_end,"-", end_p_q)
  }else{end_date <- paste0(year,"-", end_p_q)}
  
  q_to_save = corr$q_to_save[which(corr$quarters == quarter)]
  
  # SET PERIOD HERE
  cusip_subset <- cusip_all[which(cusip_all$period > beg_date & cusip_all$period < end_date ),]
  
  compustat_nearestperiod <- aggregate( compustat[c("period")], compustat[c("cusip6")], FUN=function(x) { return( max(x[x<=current]) ) } )
  compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
  ok = merge(compustat_nearestperiod, compustat, by=c("period","cusip6"))
  ok <- aggregate( ok[c("market.value.mln")], ok[c("cusip6")], FUN=sum )
  
  cusip_subset <- merge(cusip_subset, ok, by="cusip6")
  cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)
  cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)
  cusip_subset = cusip_subset[c("period","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
  cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]
  
  # activists and top20 investors only:
  list <- c(top20_percent, full_activist_list)
  
  cusip_subset <- cusip_subset[ which(cusip_subset$cik %in% list), ]
  cusip6_list <- unique(cusip_subset$cusip6)
  
  # Flag activists and top20inv
  
  act_subset = cusip_subset[ which(cusip_subset$cik %in% full_activist_list), ]
  act_subset$activist = "yes"
  
  non_act_subset = cusip_subset[ which(cusip_subset$cik %!in% full_activist_list), ]
  if ( nrow(non_act_subset) >0 ){
  non_act_subset$activist = "no"}
  
  cusip_subset = rbind(act_subset, non_act_subset)
  
  inv_subset = cusip_subset[ which(cusip_subset$cik %in% top20_percent), ]
  inv_subset$top20_investor = "yes"
  
  non_inv_subset = cusip_subset[ which(cusip_subset$cik %!in% top20_percent), ]
  non_inv_subset$top20_investor = "no"
  
  cusip_subset = rbind (inv_subset, non_inv_subset)
  
  fund_network <- data.frame(activist=character(), 
                             top20_investor=character(), 
                             s=double(),
                             # cusip6=character(),
                             num_con=integer(),
                             stringsAsFactors=FALSE) 
  fund_network_chunk <- copy(fund_network)
  
  start = 1
  end = length(cusip6_list)
  
  for ( i in start:end ) {
    cusip6_i = cusip6_list[i]
    cusip_subset_i <- cusip_subset[ which(cusip_subset$cusip6 == cusip6_i) , ]
    
    C <- unique(cusip_subset_i$market.value.mln)
    stopifnot( length(C) == 1 )
    
    num_funds_i <- length(unique(cusip_subset_i$cik))
    stopifnot( nrow(cusip_subset_i) == num_funds_i )
    list_activist_i <- cusip_subset_i$cik[which(cusip_subset_i$activist == "yes" )]
    num_activist_i = length( unique(list_activist_i)  )
    list_top20_i <- cusip_subset_i$cik[which(cusip_subset_i$top20_investor == "yes" )]
    num_top20_i = length( unique(list_top20_i)  )
    
    if( num_activist_i != 0 & num_top20_i != 0 ){
      
      print( paste(nw_date, i,cusip6_i,C,num_funds_i, num_activist_i, num_top20_i  ) )
      
      grid_funds <- expand.grid(1:num_activist_i, 1:num_top20_i)
      activist <- list_activist_i[grid_funds[,1]]
      top20_investor <- list_top20_i[grid_funds[,2]]
      
      s1 <- cusip_subset_i$value.mln[ which(cusip_subset_i$cik %in% list_activist_i)][grid_funds[,1]]
      s2 <- cusip_subset_i$value.mln[ which(cusip_subset_i$cik %in% list_top20_i)][grid_funds[,2]]
      A2 <- cusip_subset_i$total.value.mln[ which(cusip_subset_i$cik %in% list_top20_i)][grid_funds[,2]]
      
      s <- 1./( C/s1 + A2/s2 )
      fund_network_i <- data.frame( activist=activist,top20_investor=top20_investor,s=s, stringsAsFactors=FALSE )
      # fund_network_i$cusip6 <- cusip6_i
      fund_network_i$num_con <- 1
      
      fund_network_chunk <- rbind(fund_network_chunk,fund_network_i)
      
      if ( i%%20 == 0 | i == end ) {
        fund_network_chunk <- aggregate( fund_network_chunk[c("s","num_con")], fund_network_chunk[c("activist","top20_investor")], FUN=sum )
        
        fund_network <- merge(fund_network, fund_network_chunk, by=c("activist","top20_investor"), all=TRUE, suffixes=c("",".y"))
        fund_network_chunk <- fund_network_chunk[0,]
        
        fund_network$s[is.na(fund_network$s)] <- 0
        fund_network$s.y[is.na(fund_network$s.y)] <- 0
        fund_network$s <- fund_network$s + fund_network$s.y
        
        fund_network$num_con[is.na(fund_network$num_con)] <- 0
        fund_network$num_con.y[is.na(fund_network$num_con.y)] <- 0
        fund_network$num_con <- fund_network$num_con + fund_network$num_con.y
        
        fund_network <- fund_network[ , !(names(fund_network) %in% c("s.y","num_con.y"))]  
      }
      
      # fund_network <- rbind(fund_network,fund_network_i)
      
    }
  }
  
  investor_network = fund_network
  
  name = paste0( "investor_network_top20_", q_to_save, "_", year )
  
  save(investor_network, file= paste0( "C:/Users/anakhmur/Documents/Networks/Analysis/networks/", name ))
  
}



#-------------Top 10

load("top10_percent")
top10_percent = top10_percent[which(!is.na(top10_percent))]

setwd("~/Networks/Analysis")
temp.space <- new.env()
cusip_all <- get(load("cusip_ok.short", temp.space), temp.space)
rm(temp.space)


cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???
cusip_all$period <- as.Date(cusip_all$period, "%m-%d-%Y")


compustat_all <- read.csv("compustat_q_2015.csv")
compustat=compustat_all
compustat$cusip6 <- substr(compustat$cusip,1,6)
compustat$period = as.Date(as.character(compustat$datadate),"%Y%m%d")
# # creation dates:
# compustat_firstdate <- aggregate( compustat[c("datadate")], compustat[c("cusip6")], FUN=min )
# compustat_ages$date <- as.Date(as.character(compustat_ages$datadate),"%Y%m%d")

for (i in 1:nrow(compustat)) {
  if(!is.na(compustat$mkvalt[i])) {
    compustat$market.value.mln[i] <- compustat$mkvaltq[i]
  } else {
    compustat$market.value.mln[i] <- compustat$cshoq[i]*compustat$prccq[i]
  }
}

load("clean.shark.final_age")
full_activist_list <- unique(clean.shark.final$cik)
full_activist_list <- full_activist_list [which(!is.na(full_activist_list))]

quarters <- c("03-31", "06-30", "09-30", "12-31")
q_to_save <- c("1q", "2q", "3q", "4q")
beg_periods <-  c("02-28", "05-31", "08-31", "11-30")
beg_period_leaps <- c("02-29", "05-31", "08-31", "11-30")
end_periods <- c("04-30", "07-31", "10-31", "01-31")
corr <- data.frame(quarters,q_to_save, beg_periods, beg_period_leaps, end_periods)

leap_years <- c("2000", "2004", "2008", "2012", "2016")

network_dates <- sort( unique( cusip_all$period )  )
'%!in%' <- function(x,y)!('%in%'(x,y))

compustat <- unique( compustat[c("cusip6","cusip", "datadate", "market.value.mln", "period")] ) #???
compustat <- compustat[complete.cases(compustat),] #???


for ( nw  in 1:length(network_dates) ){
  
  nw_date <- network_dates[nw]
  print(nw_date)
  # SET CURRENT PERIOD!!!!! (the period of network formation )
  current = as.Date( nw_date, "%Y-%m-%d")
  
  quarter <- substr(nw_date, 6, 10)
  year <- substr(nw_date, 1, 4)
  
  if (year %in% leap_years){beg_p_q <- corr$beg_period_leaps[which(quarters == quarter)]}else{
    beg_p_q <- corr$beg_periods[which(quarters == quarter)]
  }
  
  end_p_q <- corr$end_periods[which(quarters == quarter)]
  
  beg_date <- paste0(year,"-", beg_p_q)
  if( end_p_q == "01-31" ){
    year_end = as.numeric(year)+1
    end_date <- paste0(year_end,"-", end_p_q)
  }else{end_date <- paste0(year,"-", end_p_q)}
  
  q_to_save = corr$q_to_save[which(corr$quarters == quarter)]
  
  # SET PERIOD HERE
  cusip_subset <- cusip_all[which(cusip_all$period > beg_date & cusip_all$period < end_date ),]
  
  compustat_nearestperiod <- aggregate( compustat[c("period")], compustat[c("cusip6")], FUN=function(x) { return( max(x[x<=current]) ) } )
  compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
  ok = merge(compustat_nearestperiod, compustat, by=c("period","cusip6"))
  ok <- aggregate( ok[c("market.value.mln")], ok[c("cusip6")], FUN=sum )
  
  cusip_subset <- merge(cusip_subset, ok, by="cusip6")
  cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)
  cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)
  cusip_subset = cusip_subset[c("period","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
  cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]
  
  # activists and top10 investors only:
  list <- c(top10_percent, full_activist_list)
  
  cusip_subset <- cusip_subset[ which(cusip_subset$cik %in% list), ]
  cusip6_list <- unique(cusip_subset$cusip6)
  
  # Flag activists and top10inv
  
  act_subset = cusip_subset[ which(cusip_subset$cik %in% full_activist_list), ]
  act_subset$activist = "yes"
  
  non_act_subset = cusip_subset[ which(cusip_subset$cik %!in% full_activist_list), ]
  if ( nrow(non_act_subset) >0 ){
    non_act_subset$activist = "no"}
  
  cusip_subset = rbind(act_subset, non_act_subset)
  
  inv_subset = cusip_subset[ which(cusip_subset$cik %in% top10_percent), ]
  inv_subset$top10_investor = "yes"
  
  non_inv_subset = cusip_subset[ which(cusip_subset$cik %!in% top10_percent), ]
  non_inv_subset$top10_investor = "no"
  
  cusip_subset = rbind (inv_subset, non_inv_subset)
  
  fund_network <- data.frame(activist=character(), 
                             top10_investor=character(), 
                             s=double(),
                             # cusip6=character(),
                             num_con=integer(),
                             stringsAsFactors=FALSE) 
  fund_network_chunk <- copy(fund_network)
  
  start = 1
  end = length(cusip6_list)
  
  for ( i in start:end ) {
    cusip6_i = cusip6_list[i]
    cusip_subset_i <- cusip_subset[ which(cusip_subset$cusip6 == cusip6_i) , ]
    
    C <- unique(cusip_subset_i$market.value.mln)
    stopifnot( length(C) == 1 )
    
    num_funds_i <- length(unique(cusip_subset_i$cik))
    stopifnot( nrow(cusip_subset_i) == num_funds_i )
    list_activist_i <- cusip_subset_i$cik[which(cusip_subset_i$activist == "yes" )]
    num_activist_i = length( unique(list_activist_i)  )
    list_top10_i <- cusip_subset_i$cik[which(cusip_subset_i$top10_investor == "yes" )]
    num_top10_i = length( unique(list_top10_i)  )
    
    if( num_activist_i != 0 & num_top10_i != 0 ){
      
      print( paste(nw_date, i,cusip6_i,C,num_funds_i, num_activist_i, num_top10_i  ) )
      
      grid_funds <- expand.grid(1:num_activist_i, 1:num_top10_i)
      activist <- list_activist_i[grid_funds[,1]]
      top10_investor <- list_top10_i[grid_funds[,2]]
      
      s1 <- cusip_subset_i$value.mln[ which(cusip_subset_i$cik %in% list_activist_i)][grid_funds[,1]]
      s2 <- cusip_subset_i$value.mln[ which(cusip_subset_i$cik %in% list_top10_i)][grid_funds[,2]]
      A2 <- cusip_subset_i$total.value.mln[ which(cusip_subset_i$cik %in% list_top10_i)][grid_funds[,2]]
      
      s <- 1./( C/s1 + A2/s2 )
      fund_network_i <- data.frame( activist=activist,top10_investor=top10_investor,s=s, stringsAsFactors=FALSE )
      # fund_network_i$cusip6 <- cusip6_i
      fund_network_i$num_con <- 1
      
      fund_network_chunk <- rbind(fund_network_chunk,fund_network_i)
      
      if ( i%%20 == 0 | i == end ) {
        fund_network_chunk <- aggregate( fund_network_chunk[c("s","num_con")], fund_network_chunk[c("activist","top10_investor")], FUN=sum )
        
        fund_network <- merge(fund_network, fund_network_chunk, by=c("activist","top10_investor"), all=TRUE, suffixes=c("",".y"))
        fund_network_chunk <- fund_network_chunk[0,]
        
        fund_network$s[is.na(fund_network$s)] <- 0
        fund_network$s.y[is.na(fund_network$s.y)] <- 0
        fund_network$s <- fund_network$s + fund_network$s.y
        
        fund_network$num_con[is.na(fund_network$num_con)] <- 0
        fund_network$num_con.y[is.na(fund_network$num_con.y)] <- 0
        fund_network$num_con <- fund_network$num_con + fund_network$num_con.y
        
        fund_network <- fund_network[ , !(names(fund_network) %in% c("s.y","num_con.y"))]  
      }
      
      # fund_network <- rbind(fund_network,fund_network_i)
      
    }
  }
  
  investor_network = fund_network
  
  name = paste0( "investor_network_top10_", q_to_save, "_", year )
  
  save(investor_network, file= paste0( "C:/Users/anakhmur/Documents/Networks/Analysis/networks/", name ))
  
}


#---------------TOP 5 

rm(list=ls())
gc()

library(data.table)

load("top5_percent_old")
top5_percent = top5_percent[which(!is.na(top5_percent))]

setwd("~/Networks/Analysis")
temp.space <- new.env()
cusip_all <- get(load("cusip_ok.short", temp.space), temp.space)
rm(temp.space)


cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???
cusip_all$period <- as.Date(cusip_all$period, "%m-%d-%Y")


compustat_all <- read.csv("compustat_q_2015.csv")
compustat=compustat_all
compustat$cusip6 <- substr(compustat$cusip,1,6)
compustat$period = as.Date(as.character(compustat$datadate),"%Y%m%d")
# # creation dates:
# compustat_firstdate <- aggregate( compustat[c("datadate")], compustat[c("cusip6")], FUN=min )
# compustat_ages$date <- as.Date(as.character(compustat_ages$datadate),"%Y%m%d")

for (i in 1:nrow(compustat)) {
  if(!is.na(compustat$mkvalt[i])) {
    compustat$market.value.mln[i] <- compustat$mkvaltq[i]
  } else {
    compustat$market.value.mln[i] <- compustat$cshoq[i]*compustat$prccq[i]
  }
}

load("clean.shark.final_age")
full_activist_list <- unique(clean.shark.final$cik)
full_activist_list <- full_activist_list [which(!is.na(full_activist_list))]

quarters <- c("03-31", "06-30", "09-30", "12-31")
q_to_save <- c("1q", "2q", "3q", "4q")
beg_periods <-  c("02-28", "05-31", "08-31", "11-30")
beg_period_leaps <- c("02-29", "05-31", "08-31", "11-30")
end_periods <- c("04-30", "07-31", "10-31", "01-31")
corr <- data.frame(quarters,q_to_save, beg_periods, beg_period_leaps, end_periods)

leap_years <- c("2000", "2004", "2008", "2012", "2016")

network_dates <- sort( unique( cusip_all$period )  )
'%!in%' <- function(x,y)!('%in%'(x,y))

compustat <- unique( compustat[c("cusip6","cusip", "datadate", "market.value.mln", "period")] ) #???
compustat <- compustat[complete.cases(compustat),] #???


for ( nw  in 1:length(network_dates) ){
  
  nw_date <- network_dates[nw]
  print(nw_date)
  # SET CURRENT PERIOD!!!!! (the period of network formation )
  current = as.Date( nw_date, "%Y-%m-%d")
  
  quarter <- substr(nw_date, 6, 10)
  year <- substr(nw_date, 1, 4)
  
  if (year %in% leap_years){beg_p_q <- corr$beg_period_leaps[which(quarters == quarter)]}else{
    beg_p_q <- corr$beg_periods[which(quarters == quarter)]
  }
  
  end_p_q <- corr$end_periods[which(quarters == quarter)]
  
  beg_date <- paste0(year,"-", beg_p_q)
  if( end_p_q == "01-31" ){
    year_end = as.numeric(year)+1
    end_date <- paste0(year_end,"-", end_p_q)
  }else{end_date <- paste0(year,"-", end_p_q)}
  
  q_to_save = corr$q_to_save[which(corr$quarters == quarter)]
  
  # SET PERIOD HERE
  cusip_subset <- cusip_all[which(cusip_all$period > beg_date & cusip_all$period < end_date ),]
  
  compustat_nearestperiod <- aggregate( compustat[c("period")], compustat[c("cusip6")], FUN=function(x) { return( max(x[x<=current]) ) } )
  compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
  ok = merge(compustat_nearestperiod, compustat, by=c("period","cusip6"))
  ok <- aggregate( ok[c("market.value.mln")], ok[c("cusip6")], FUN=sum )
  
  cusip_subset <- merge(cusip_subset, ok, by="cusip6")
  cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)
  cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)
  cusip_subset = cusip_subset[c("period","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
  cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]
  
  # activists and top5 investors only:
  list <- c(top5_percent, full_activist_list)
  
  cusip_subset <- cusip_subset[ which(cusip_subset$cik %in% list), ]
  cusip6_list <- unique(cusip_subset$cusip6)
  
  # Flag activists and top5inv
  
  act_subset = cusip_subset[ which(cusip_subset$cik %in% full_activist_list), ]
  act_subset$activist = "yes"
  
  non_act_subset = cusip_subset[ which(cusip_subset$cik %!in% full_activist_list), ]
  if ( nrow(non_act_subset) >0 ){
    non_act_subset$activist = "no"}
  
  cusip_subset = rbind(act_subset, non_act_subset)
  
  inv_subset = cusip_subset[ which(cusip_subset$cik %in% top5_percent), ]
  inv_subset$top5_investor = "yes"
  
  non_inv_subset = cusip_subset[ which(cusip_subset$cik %!in% top5_percent), ]
  non_inv_subset$top5_investor = "no"
  
  cusip_subset = rbind (inv_subset, non_inv_subset)
  
  fund_network <- data.frame(activist=character(), 
                             top5_investor=character(), 
                             s=double(),
                             # cusip6=character(),
                             num_con=integer(),
                             stringsAsFactors=FALSE) 
  fund_network_chunk <- copy(fund_network)
  
  start = 1
  end = length(cusip6_list)
  
  for ( i in start:end ) {
    cusip6_i = cusip6_list[i]
    cusip_subset_i <- cusip_subset[ which(cusip_subset$cusip6 == cusip6_i) , ]
    
    C <- unique(cusip_subset_i$market.value.mln)
    stopifnot( length(C) == 1 )
    
    num_funds_i <- length(unique(cusip_subset_i$cik))
    stopifnot( nrow(cusip_subset_i) == num_funds_i )
    list_activist_i <- cusip_subset_i$cik[which(cusip_subset_i$activist == "yes" )]
    num_activist_i = length( unique(list_activist_i)  )
    list_top5_i <- cusip_subset_i$cik[which(cusip_subset_i$top5_investor == "yes" )]
    num_top5_i = length( unique(list_top5_i)  )
    
    if( num_activist_i != 0 & num_top5_i != 0 ){
      
      print( paste(nw_date, i,cusip6_i,C,num_funds_i, num_activist_i, num_top5_i  ) )
      
      grid_funds <- expand.grid(1:num_activist_i, 1:num_top5_i)
      activist <- list_activist_i[grid_funds[,1]]
      top5_investor <- list_top5_i[grid_funds[,2]]
      
      s1 <- cusip_subset_i$value.mln[ which(cusip_subset_i$cik %in% list_activist_i)][grid_funds[,1]]
      s2 <- cusip_subset_i$value.mln[ which(cusip_subset_i$cik %in% list_top5_i)][grid_funds[,2]]
      A2 <- cusip_subset_i$total.value.mln[ which(cusip_subset_i$cik %in% list_top5_i)][grid_funds[,2]]
      
      s <- 1./( C/s1 + A2/s2 )
      fund_network_i <- data.frame( activist=activist,top5_investor=top5_investor,s=s, stringsAsFactors=FALSE )
      # fund_network_i$cusip6 <- cusip6_i
      fund_network_i$num_con <- 1
      
      fund_network_chunk <- rbind(fund_network_chunk,fund_network_i)
      
      if ( i%%20 == 0 | i == end ) {
        fund_network_chunk <- aggregate( fund_network_chunk[c("s","num_con")], fund_network_chunk[c("activist","top5_investor")], FUN=sum )
        
        fund_network <- merge(fund_network, fund_network_chunk, by=c("activist","top5_investor"), all=TRUE, suffixes=c("",".y"))
        fund_network_chunk <- fund_network_chunk[0,]
        
        fund_network$s[is.na(fund_network$s)] <- 0
        fund_network$s.y[is.na(fund_network$s.y)] <- 0
        fund_network$s <- fund_network$s + fund_network$s.y
        
        fund_network$num_con[is.na(fund_network$num_con)] <- 0
        fund_network$num_con.y[is.na(fund_network$num_con.y)] <- 0
        fund_network$num_con <- fund_network$num_con + fund_network$num_con.y
        
        fund_network <- fund_network[ , !(names(fund_network) %in% c("s.y","num_con.y"))]  
      }
      
      # fund_network <- rbind(fund_network,fund_network_i)
      
    }
  }
  
  investor_network = fund_network
  
  name = paste0( "investor_network_top5_new_", q_to_save, "_", year )
  
  save(investor_network, file= paste0( "C:/Users/anakhmur/Documents/Networks/Analysis/networks/", name ))
  
}
