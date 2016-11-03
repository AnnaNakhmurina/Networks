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

# SET PERIOD HERE

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
  
  print( paste(nw_date, i,cusip6_i,C,num_funds_i,num_funds_i ) )
  
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


name = paste0( "fund_network_", q_to_save, "_", year )

save(fund_network, file= paste0( "C:/Users/anakhmur/Documents/Networks/Analysis/networks/", name ))

}



#-------------------------------Summary statistics
# read all funds networks 
rm(list=ls())
gc()

# Create summary statistics regarding the fund networks
nw_files = list.files(path = "C:/Users/anakhmur/Documents/Networks/Analysis/networks", pattern = "fund_network")

setwd("~/Networks/Analysis/networks")
fund_n <- data.frame()
for (file in nw_files){
  print(file)
  load(file)

fund_n <- rbind(fund_n, fund_network)
}

# delete duplicate funds
fn <- fund_n[which(fund_n$fund1 != fund_n$fund2),]

out <- data.frame()
simple <- cbind( min(fn$num_con), quantile(fn$num_con, 0.25), 
                 mean(fn$num_con),median(fn$num_con), quantile(fn$num_con, 0.75),max(fn$num_con), sd(fn$num_con) )
simple = round(simple,  digits=2)
simple = cbind( c("# of connections fund"), simple)
complex <- cbind( min(fn$s), quantile(fn$s, 0.25), 
                  mean(fn$s),median(fn$s), quantile(fn$s, 0.75),max(fn$s), sd(fn$s))
complex = round(complex, digits =2)
complex =  cbind( c("spring fund") , complex)
out <- rbind(complex, simple)
row.names(out) <- NULL
out <- as.data.frame( out)
names(out) <- c("name", "min","25%","Mean", "Median", "75%", "max", "Sd")


# networks_summary_2015 = out
# save(networks_summary_2015, file="networks_summary_2015")

fund_networks_summary_all = out
save(fund_networks_summary_all, file="fund_networks_summary_all")


#------------------------------------ADD CENTRALITY MEASURES TO THE FUND NETWORK DATA--------------------------------------

library(igraph)
library(statnet)
library(network)
library(sna)

centrality_table = function ( fund_network ){
  
  simple = fund_network[c("fund1","fund2")]
  connections_number = fund_network[c("fund1","fund2","num_con")]
  spring = fund_network[c("fund1","fund2","s")]
  
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


nw_files = list.files(path = "C:/Users/anakhmur/Documents/Networks/Analysis/networks", pattern = "fund_network")
# file=nw_files[1]
setwd("~/Networks/Analysis/networks")

# Create corespondence table 

quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)

centrality_summary = data.frame()

for (file in nw_files){

q = substr(file, 14,15)
y = substr(file, 17, 20)
qc = corr$period[which(corr$quarter == q)]
period = paste0(qc,"-",y)

load(file)
summary_1q = centrality_table(fund_network)
summary_1q$period = period
print(c(file,period))
centrality_summary  = rbind(centrality_summary, summary_1q)

}

centrality_summary$date = as.Date(centrality_summary$period, "%m-%d-%Y")
fund_centrality_summary = centrality_summary
setwd("~/Networks/Analysis")
save(fund_centrality_summary, file="fund_centrality_summary")

# Now add centralities to the data 

rm(list=ls())
gc()

library(data.table)
setwd("~/Networks/Analysis")

# load("13f_2000_2014_cik_dt")
# load("fund_centrality_summary")
# cik_13f = merge(cik_13f, fund_centrality_summary, by =c("cik", "date"))
# save(cik_13f, file="13f_2000_2014_w_centr")
# 

# Add centrality data to the cusip ok database

# load("cusip_ok.short")
# 
# drops = c("simple_degree", "simple_clos", "simple_between", 
#                       "simple_bonacich", "con_degree", "con_between", "con_clos", 
#                       "con_bonacich", "spring_degree", "spring_between", "spring_clos", "spring_bonacich")
# 
# c=cusip_ok.short[ , !(names(cusip_ok.short) %in% drops)]
# 
# cusip_ok.short = merge(c,fund_centrality_summary, by =c("cik", "period"))
# save(cusip_ok.short, file="cusip_ok.short_w_centr")
