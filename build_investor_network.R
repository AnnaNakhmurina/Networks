rm(list=ls())
gc()

library(data.table)

# First, create a list of funds who are in the top-20% of reporters in each of the reporting periods

load("top20_percent")
top20_percent = top20_percent[which(!is.na(top20_percent))]

temp.space <- new.env()
cusip_all <- get(load("13f_2000_2014_cik_dt", temp.space), temp.space)
# cusip_all <- get(load("dt_13", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???
# cusip_all$period <- as.Date(cusip_all$period, "%m-%d-%Y")

load("compustat_short_2000_2014")

compustat <- unique( compustat[c("cusip6","cusip", "datadate", "market.value.mln", "period")] ) #???
compustat <- compustat[complete.cases(compustat),] #???

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

network_dates <- sort( unique( cusip_all$date )  )
'%!in%' <- function(x,y)!('%in%'(x,y))

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
cusip_subset <- cusip_all[which(cusip_all$date > beg_date & cusip_all$date < end_date ),]

compustat_nearestperiod <- aggregate( compustat[c("period")], compustat[c("cusip6")], FUN=function(x) { return( max(x[x<=current]) ) } )
compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
ok = merge(compustat_nearestperiod, compustat, by=c("period","cusip6"))
ok <- aggregate( ok[c("market.value.mln")], ok[c("cusip6")], FUN=sum )

cusip_subset <- merge(cusip_subset, ok, by="cusip6")
cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)
cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)
cusip_subset = cusip_subset[c("date","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]

# activists and top20 investors only:
list <- c(top20_percent, full_activist_list)

cusip_subset <- cusip_subset[ which(cusip_subset$cik %in% list), ]
cusip6_list <- unique(cusip_subset$cusip6)

# Flag activists and top20inv

act_subset = cusip_subset[ which(cusip_subset$cik %in% full_activist_list), ]
act_subset$activist = "yes"

non_act_subset = cusip_subset[ which(cusip_subset$cik %!in% full_activist_list), ]
non_act_subset$activist = "no"

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


#-------- Build top 10 network 


load("top10_percent")
top10_percent = top10_percent[which(!is.na(top10_percent))]


temp.space <- new.env()
cusip_all <- get(load("13f_2000_2014_cik_dt", temp.space), temp.space)
# cusip_all <- get(load("dt_13", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???
# cusip_all$period <- as.Date(cusip_all$period, "%m-%d-%Y")

load("compustat_short_2000_2014")

compustat <- unique( compustat[c("cusip6","cusip", "datadate", "market.value.mln", "period")] ) #???
compustat <- compustat[complete.cases(compustat),] #???

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

network_dates <- sort( unique( cusip_all$date )  )
'%!in%' <- function(x,y)!('%in%'(x,y))

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
  cusip_subset <- cusip_all[which(cusip_all$date > beg_date & cusip_all$date < end_date ),]
  
  compustat_nearestperiod <- aggregate( compustat[c("period")], compustat[c("cusip6")], FUN=function(x) { return( max(x[x<=current]) ) } )
  compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
  ok = merge(compustat_nearestperiod, compustat, by=c("period","cusip6"))
  ok <- aggregate( ok[c("market.value.mln")], ok[c("cusip6")], FUN=sum )
  
  cusip_subset <- merge(cusip_subset, ok, by="cusip6")
  cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)
  cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)
  cusip_subset = cusip_subset[c("date","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
  cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]
  
  # activists and top10 investors only:
  list <- c(top10_percent, full_activist_list)
  
  cusip_subset <- cusip_subset[ which(cusip_subset$cik %in% list), ]
  cusip6_list <- unique(cusip_subset$cusip6)
  
  # Flag activists and top10inv
  
  act_subset = cusip_subset[ which(cusip_subset$cik %in% full_activist_list), ]
  act_subset$activist = "yes"
  
  non_act_subset = cusip_subset[ which(cusip_subset$cik %!in% full_activist_list), ]
  non_act_subset$activist = "no"
  
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


# -------------------Build top 5 network


load("top5_percent_old")
top5_percent = top5_percent[which(!is.na(top5_percent))]

temp.space <- new.env()
cusip_all <- get(load("13f_2000_2014_cik_dt", temp.space), temp.space)
# cusip_all <- get(load("dt_13", temp.space), temp.space)
rm(temp.space)

cusip_all <- data.frame(cusip_all)
cusip_all <- cusip_all[rowSums(is.na(cusip_all))<ncol(cusip_all),] #???
# cusip_all$period <- as.Date(cusip_all$period, "%m-%d-%Y")

load("compustat_short_2000_2014")

compustat <- unique( compustat[c("cusip6","cusip", "datadate", "market.value.mln", "period")] ) #???
compustat <- compustat[complete.cases(compustat),] #???

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

network_dates <- sort( unique( cusip_all$date )  )
'%!in%' <- function(x,y)!('%in%'(x,y))

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
  cusip_subset <- cusip_all[which(cusip_all$date > beg_date & cusip_all$date < end_date ),]
  
  compustat_nearestperiod <- aggregate( compustat[c("period")], compustat[c("cusip6")], FUN=function(x) { return( max(x[x<=current]) ) } )
  compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
  ok = merge(compustat_nearestperiod, compustat, by=c("period","cusip6"))
  ok <- aggregate( ok[c("market.value.mln")], ok[c("cusip6")], FUN=sum )
  
  cusip_subset <- merge(cusip_subset, ok, by="cusip6")
  cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)
  cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)
  cusip_subset = cusip_subset[c("date","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
  cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]
  
  # activists and top5 investors only:
  list <- c(top5_percent, full_activist_list)
  
  cusip_subset <- cusip_subset[ which(cusip_subset$cik %in% list), ]
  cusip6_list <- unique(cusip_subset$cusip6)
  
  # Flag activists and top5inv
  
  act_subset = cusip_subset[ which(cusip_subset$cik %in% full_activist_list), ]
  act_subset$activist = "yes"
  
  non_act_subset = cusip_subset[ which(cusip_subset$cik %!in% full_activist_list), ]
  non_act_subset$activist = "no"
  
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



#----------------------------------Do summary statistics table

# read all funds networks 
rm(list=ls())
gc()

# Create summary statistics regarding the fund networks
nw_files = list.files(path = "C:/Users/anakhmur/Documents/Networks/Analysis/networks", pattern = "investor_network_top5")

setwd("~/Networks/Analysis/networks")
fund_n <- data.frame()
for (file in nw_files){
  print(file)
  load(file)
  
  fund_n <- rbind(fund_n, investor_network)
}

# delete duplicate funds
fn <- fund_n

out <- data.frame()
simple <- cbind( mean(fn$num_con), sd(fn$num_con), 
                 min(fn$num_con),quantile(fn$num_con, 0.25), median(fn$num_con), quantile(fn$num_con, 0.75),max(fn$num_con) )
simple = round(simple,  digits=2)
simple = cbind( c("# of connections fund"), simple)
complex <- cbind( mean(fn$s), sd(fn$s), 
                  min(fn$s),quantile(fn$s, 0.25), median(fn$s), quantile(fn$s, 0.75),max(fn$s) )
complex = round(complex, digits =2)
complex =  cbind( c("spring fund") , complex)
out <- rbind(complex, simple)
row.names(out) <- NULL
out <- as.data.frame( out)
names(out) <- c("name", "mean","sd","min", "25%", "median%", "75%", "max")


# networks_summary_2015 = out
# save(networks_summary_2015, file="networks_summary_2015")

investor_networks_summary_all = out
save(investor_networks_summary_all, file="investor_networks_summary_all_top5")

#------------------------------------ADD CENTRALITY MEASURES TO THE FUND NETWORK DATA--------------------------------------

library(igraph)
library(statnet)
library(network)
library(sna)

centrality_table = function ( fund_network ){
  
  simple = fund_network[c("activist","top20_investor")]
  connections_number = fund_network[c("activist","top20_investor","num_con")]
  spring = fund_network[c("activist","top20_investor","s")]
  
  net_simple <- graph_from_data_frame(d=simple, directed=F) 
  net_connections_number  <- graph_from_data_frame(d=connections_number, directed=F) 
  net_spring <- graph_from_data_frame(d=spring, directed=T) 
  
  simple_adjnet = as_adjacency_matrix(net_simple)
  con_adjnet = as_adjacency_matrix(net_connections_number)
  con_spring = as_adjacency_matrix(net_spring)
  
  simple_am = as.matrix(simple_adjnet)
  con_am = as.matrix(simple_adjnet)
  spring_am = as.matrix(con_spring)
  
  simple_degree=degree(simple_am)
  con_degree = degree(con_am)
  spring_degree = degree(spring_am)
  
  names(simple_degree)=names(V(net_simple))
  names(con_degree)=names(V(net_connections_number))
  names(spring_degree)=names(V(net_spring))
  
  simple_between=betweenness(simple_am)
  con_between=betweenness(con_am)
  spring_between=betweenness(spring_am)
  
  names(simple_between)=names(V(net_simple))
  names(con_between)=names(V(net_connections_number))
  names(spring_between)=names(V(net_spring))
  
  simple_clos=closeness(simple_am)
  con_clos=closeness(con_am)
  spring_clos=closeness(spring_am)
  
  names(simple_clos)=names(V(net_simple))
  names(con_clos)=names(V(net_connections_number))
  names(spring_clos)=names(V(net_spring))
  
  simple_bonacich=power_centrality(net_simple, nodes = V(net_simple), loops = FALSE, exponent = 1,
                                   rescale = FALSE, tol = 1e-07, sparse = TRUE)
  con_bonacich=power_centrality(net_connections_number, nodes = V(net_connections_number), loops = FALSE, exponent = 1,
                                rescale = FALSE, tol = 1e-07, sparse = TRUE)
  spring_bonacich=power_centrality(net_spring, nodes = V(net_spring), loops = FALSE, exponent = 1,
                                   rescale = FALSE, tol = 1e-07, sparse = TRUE)
  
  
  summary = cbind(simple_degree,simple_clos,simple_between,simple_bonacich,
                  con_degree,con_between,con_clos,con_bonacich,
                  spring_degree,spring_between,spring_clos,spring_bonacich)
  summary= as.data.frame(summary)
  summary$cik = rownames(summary)
  rownames(summary) <- NULL
  return(summary)
}


nw_files = list.files(path = "C:/Users/anakhmur/Documents/Networks/Analysis/networks", pattern = "investor_network")
file=nw_files[1]
setwd("~/Networks/Analysis/networks")

# Create corespondence table 

quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)

centrality_summary = data.frame()

for (file in nw_files){
  
  q = substr(file, 18,19)
  y = substr(file, 21, 24)
  qc = corr$period[which(corr$quarter == q)]
  period = paste0(qc,"-",y)
  
  load(file)
  summary_1q = centrality_table(investor_network)
  summary_1q$period = period
  print(c(file,period))
  centrality_summary  = rbind(centrality_summary, summary_1q)
  
}

centrality_summary$date = as.Date(centrality_summary$period, "%m-%d-%Y")
investor_centrality_summary = centrality_summary
setwd("~/Networks/Analysis")

names( investor_centrality_summary ) = c("inv_simple_degree", "inv_simple_clos", "inv_simple_between", 
                                         "inv_simple_bonacich", "inv_con_degree", "inv_con_between",
                                         "inv_con_clos", "inv_con_bonacich", "inv_spring_degree",
                                         "inv_spring_between", "inv_spring_clos", "inv_spring_bonacich",
                                          "cik", "period","date"  )

save(investor_centrality_summary, file="investor_centrality_summary")


# -------------------- ADD CENTRALITIES TO THE 13f DATA (MAKE SURE TO RUN THIS CODE AFTER THE FUND CENTRALITIES ARE ADDED)

rm(list=ls())
gc()

library(data.table)
setwd("~/Networks/Analysis")

load("13f_2000_2014_w_centr")

load("investor_centrality_summary")

cik_13f = merge( cik_13f, investor_centrality_summary, by =c("cik", "date", "period") )
# save(cik_13f, file="13f_2000_2014_w_both_centr")
# save(cik_13f, file="13f_2000_2014_w_both_centr_new")
#
# load("13f_2000_2014_w_both_centr")
# load("13f_2000_2014_w_both_centr_new")

library(data.table)

# length 1510738

cik_13f = as.data.table(cik_13f)

cik_13f$share =  cik_13f$value.mln.all/cik_13f$total.value

cik_13f = cik_13f[ , sd_weight :=( (share -mean(share))/sd(share) ) , by=c("date", "cik")]

cik_13f = cik_13f[ , norm_weight := (share - min(share))/(max(share) - min(share) ) , by=c("date", "cik")]

# save(cik_13f, file="13f_2000_2014_w_both_centr")
# save(cik_13f, file="13f_2000_2014_w_both_centr_new")


# Add centrality data to the cusip ok database

load("cusip_ok.short_w_centr")
#

cusip_ok.short = merge( cusip_ok.short, investor_centrality_summary, by =c("cik", "period","date") )
save(cusip_ok.short, file="cusip_ok.short_w_both_centr")

load("cusip_ok.short_w_both_centr")
library(data.table)

# length 1510738

cusip_ok.short = as.data.table(cusip_ok.short)

cusip_ok.short$share =  cusip_ok.short$value.mln.all/cusip_ok.short$total.value

cusip_ok.short = cusip_ok.short[ ,sd_weight := (share -mean(share))/sd(share) , by=c("date", "cik")]

cusip_ok.short = cusip_ok.short[ , norm_weight := (share - min(share))/(max(share) - min(share) ) , by=c("date", "cik")]
save(cusip_ok.short, file="cusip_ok.short_w_both_centr")
