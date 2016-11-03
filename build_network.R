rm(list=ls())
gc()

library(data.table)

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
# #??????????????????
# compustat$year <- compustat$datadate%/%10000
# # compustat <- aggregate( compustat$market.value.mln, compustat[c("cusip6", "datadate")], FUN=sum )
# compustat <- compustat[which(compustat$year==2015),]
# compustat <- aggregate( compustat[c("market.value.mln")], compustat[c("cusip6")], FUN=sum )

cusip_subset <- merge(cusip_subset, ok, by="cusip6")
# cusip_subset <- merge(cusip_subset, compustat, by=c("datadate", "cusip6"))
# rm(compustat)
cusip_subset$value.mln <- as.numeric(cusip_subset$value.mln.all)/1000.
cusip_subset$total.value.mln <- as.numeric(cusip_subset$total.value)/1000.

cusip_subset <- cusip_subset[c("datadate","cusip6","cik","total.value.mln","value.mln","market.value.mln")]
cusip_subset <- cusip_subset[ order(cusip_subset$cusip6), ]

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
  
  print( paste(nw_date, i,cusip6_i,C,num_funds_i) )
  
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



# save(fund_network, file="fund_network_1q_2015")
#Delete duplicate funds
# load("fund_network_2q_2015")

# fund_network <- fund_network[which(fund_network$fund1 != fund_network$fund2),]

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

# Summary statistics with zeroes
fund_network_wzeros1=fund_network_wzeros

# Create an edge list 
library(igraph)
library(statnet)
library(network)
load("fund_network_1q_2015")

load("fund_network_wzeros_1q_2015")

simple = fund_network_wzeros[c("fund1","fund2")]
connections_number = fund_network[c("fund1","fund2","num_con")]
spring = fund_network[c("fund1","fund2","s")]

# 
# 
# 
# net <- as.network(x = simple, # the network object
#                   directed = FALSE, # specify whether the network is directed
#                   loops = FALSE, # do we allow self ties (should not allow them)
#                   multiple = FALSE,
#                   matrix.type = "edgelist" # the type of input
# )
# 
# 
# edge_weights <- fund_network$s
# set.edge.value(net,"trust",edge_weights)
# 
# plot.network(net)


test = connections_number
nrow(test); nrow(unique(test[,c("fund1", "fund2")]))

net <- graph_from_data_frame(d=test, directed=F) 

# Examine the resulting object:
class(net)
net 

# We can access the nodes, edges, and their attributes:
E(net)
V(net)
E(net)$num_con

# If you need them, you can extract an edge list 
# or a matrix back from the igraph networks.
as_edgelist(net, names=T)
am=as_adjacency_matrix(net, attr="num_con")

vector = matrix(1, nrow = 117, ncol = 1)

out = t(vector) %*% am

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")


# You can also look at the network matrix directly:
net[1,]
net[5,7]

# First attempt to plot the graph:
plot(net) # not pretty!

# Removing loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# Let's and reduce the arrow size and remove the labels:
plot(net, edge.arrow.size=.4,vertex.label=NA)


# Plot with curved edges (edge.curved=.1) and reduce arrow size:
# Note that using curved edges will allow you to see multiple links
# between two nodes (e.g. links going in either direction, or multiplex links)
plot(net, edge.arrow.size=.4, edge.curved=.1)


plot(net, edge.arrow.size=.4, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 

l <- layout_in_circle(net)
plot(net, layout=l,edge.arrow.size=.4,vertex.label=NA)
