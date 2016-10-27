rm(list=ls())
gc()

library(data.table)

setwd("~/Networks/Analysis")
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

load("clean.shark.final")
full_activist_list <- unique(clean.shark.final$cik)
full_activist_list <- full_activist_list [which(!is.na(full_activist_list))]

# SET PERIOD HERE
cusip_subset <- cusip_all[which(cusip_all$date > "2007-2-28" & cusip_all$date < "2007-4-30"),]

# SET CURRENT PERIOD!!!!! (the period of network formation )

current = as.Date( "2007-3-31", "%Y-%m-%d")

compustat_nearestperiod <- aggregate( compustat[c("period")], compustat[c("cusip6")], FUN=function(x) { return( max(x[x<=current]) ) } )
compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
ok = merge(compustat_nearestperiod, compustat, by=c("period","cusip6"))
ok <- aggregate( ok[c("market.value.mln")], ok[c("cusip6")], FUN=sum )

cusip_subset <- merge(cusip_subset, ok, by="cusip6")
cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)
cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)
cusip_subset = cusip_subset[c("date","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]

# activists only:
cusip_subset <- cusip_subset[ which(cusip_subset$cik %in% full_activist_list), ]
cusip6_list <- unique(cusip_subset$cusip6)

fund_network <- data.frame(fund1=character(), 
                           fund2=character(), 
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
  
  print( paste(i,cusip6_i,C,num_funds_i) )
  
  grid_funds <- expand.grid(1:num_funds_i, 1:num_funds_i)
  fund1 <- cusip_subset_i$cik[grid_funds[,1]]
  fund2 <- cusip_subset_i$cik[grid_funds[,2]]
  s1 <- cusip_subset_i$value.mln[grid_funds[,1]]
  s2 <- cusip_subset_i$value.mln[grid_funds[,2]]
  A2 <- cusip_subset_i$total.value.mln[grid_funds[,2]]
  
  s <- 1./( C/s1 + A2/s2 )
  fund_network_i <- data.frame( fund1=fund1,fund2=fund2,s=s, stringsAsFactors=FALSE )
  # fund_network_i$cusip6 <- cusip6_i
  fund_network_i$num_con <- 1
  
  fund_network_chunk <- rbind(fund_network_chunk,fund_network_i)
  
  if ( i%%20 == 0 | i == end ) {
    fund_network_chunk <- aggregate( fund_network_chunk[c("s","num_con")], fund_network_chunk[c("fund1","fund2")], FUN=sum )
    
    fund_network <- merge(fund_network, fund_network_chunk, by=c("fund1","fund2"), all=TRUE, suffixes=c("",".y"))
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

save(fund_network, file="fund_network_1q_2007")

#-------------------------------Delete duplicate funds
load("fund_network_2q_2015")

fund_network <- fund_network[which(fund_network$fund1 != fund_network$fund2),]

# Add zeros connections to the network to keep the structure of the network complete
full_activist_list = sort( full_activist_list[!is.na(full_activist_list)] )
all_pair_combinations = expand.grid(full_activist_list, full_activist_list)
names(all_pair_combinations) <- c("fund1", "fund2" )
# Delete duplicate funds
all_pair_combinations <- all_pair_combinations[which(all_pair_combinations$fund1 != all_pair_combinations$fund2),]

pair = a[1,]
test = a[1:5,]

add_zeroes <- function(x){
  pair = all_pair_combinations[x,]
  if( nrow(merge(pair,fund_network[,1:2]) )<=0 ){ 
    out_zero <- cbind(pair, 0,0)
    names(out_zero) = names(fund_network)
  }else(out_zero=data.frame())
  
  return(out_zero)
}

wzeroes = do.call( rbind, lapply(1:nrow(all_pair_combinations),add_zeroes ) )

fund_network_wzeros = rbind(fund_network,wzeroes)
save(fund_network_wzeros, file="fund_network_wzeros_1q_2015")

# Summary statistics

fund_network1 = fund_network
fund_network2 = fund_network

fund_n <- rbind(fund_network1, fund_network2)
load("fund_network_3q_2015")
fund_n <- rbind(fund_n, fund_network)
load("fund_network_4q_2015")
fund_n <- rbind(fund_n, fund_network)

# Create summary statistics regarding the fund networks

# delete duplicate funds
fn <- fund_n[which(fund_n$fund1 != fund_n$fund2),]

out <- data.frame()
simple <- cbind( min(fn$num_con), quantile(fn$num_con, 0.25), 
                 mean(fn$num_con),median(fn$num_con), quantile(fn$num_con, 0.75),max(fn$num_con), sd(fn$num_con) )
simple = round(simple,  digits=2)
simple = cbind( c("number of connections"), simple)
complex <- cbind( min(fn$s), quantile(fn$s, 0.25), 
                  mean(fn$s),median(fn$s), quantile(fn$s, 0.75),max(fn$s), sd(fn$s))
complex = round(complex, digits =2)
complex =  cbind( c("spring measure") , complex)
out <- rbind(complex, simple)
row.names(out) <- NULL
out <- as.data.frame( out)
names(out) <- c("name", "min","25%","Mean", "Median", "75%", "max", "Sd")

networks_summary_2015 = out
save(networks_summary_2015, file="networks_summary_2015")

