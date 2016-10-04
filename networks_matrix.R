rm(list=ls())
gc()



library(dtplyr)
library(plyr)
library(dplyr)
library(data.table)
# This file creates a network (adjacency) matrix

setwd("~/Networks/Data/13f_test")

load("cusip_ok.short")

cusip_ok.short <- data.frame(cusip_ok.short)
cusip_ok.short <- cusip_ok.short[rowSums(is.na(cusip_ok.short))<ncol(cusip_ok.short),]

cusip_ok.short$period = as.Date(cusip_ok.short$period, "%m-%d-%Y")
cusip_ok.2015 = cusip_ok.short[which(cusip_ok.short$period > "2014-12-31" & cusip_ok.short$period < "2016-01-01"),]

fund.list <- unique(cusip_ok.short$cik)

# Merge the database with compustat to get mkt cap
setwd("~/Networks")
compustat <- read.csv("compustat_2014.csv")
compustat$cusip6 <-  substr(compustat$cusip, 1,6)
compustat.short <- compustat[c("cusip6","mkvalt", "csho", "prcc_f", "datadate")]


a = filter(compustat.short, cusip6 == "03761U" & cik == "902584")

for (i in 1:nrow(compustat.short)){
if(!is.na(compustat.short$mkvalt[i])){
  compustat.short$market.value.mln[i] <- compustat.short$mkvalt[i]

    }else{compustat.short$market.value.mln[i] <- compustat.short$csho[i]*compustat.short$prcc_f[i]}
  
}

# merge databases
compustat.short = unique( compustat.short[c("cusip6", "datadate", "market.value.mln")] )
compustat.short = compustat.short[complete.cases(compustat.short),]

comp_13f.all = merge(cusip_ok.2015, compustat.short, by=c("datadate", "cusip6"))
comp_13f.all$value.mln = as.numeric(comp_13f.all$value.mln.all)/1000
comp_13f.all$total.value.mln = as.numeric(comp_13f.all$total.value)/1000
# Create activist list

setwd("~/Networks")
load("clean.shark.partial")
full.activist.list <- unique(clean.shark.partial$cik)
activist.list.in13f = full.activist.list[which(full.activist.list %in% fund.list)]


# The first thing to look at would be an activist network. Create it!

# ---- The simplest network
quarter = "2015-03-31"

comp_13f = filter(comp_13f.all, period==quarter )

activist.network.spring <- data.frame()
activist.network <- data.frame()

for(i in 1:length(activist.list.in13f) ){
  print(i)
  node1 <-activist.list.in13f[i]
  
   node1_data <- comp_13f[which(comp_13f$cik == node1),]
   
  if(nrow(node1_data) >0){
  node1_list <- node1_data$cusip6
  other.fund.list <- activist.list.in13f [! activist.list.in13f %in% activist.list.in13f[i]]
  
  new_table <- comp_13f[which(comp_13f$cusip6 %in% node1_list),]
  new_data.table <- as.data.table(new_table)
  node2.list <- unique(new_table$cik)[!unique(new_table$cik) %in% node1]
  
  
  networks_table <- data.frame()
  spring_table <- data.frame()
  
  for (k in 1:length(node2.list)){
    
    node2 <- node2.list[k]
    node2.table = filter(new_table, cik == node2)
    number_con <-   length(  unique( node2.table$cusip  ))
    out <- cbind(node1, node2,number_con)
    
    # check whether the connection already exists
    
    to.check <- as.data.frame( cbind( node2, node1, number_con) )
    names(to.check) <- c("node1", "node2", "number_con")
    
    if (nrow( match_df(activist.network, to.check) ) == 0){ networks_table <- rbind(networks_table,out) }
    
    
    node1.node2.table = filter(new_table, cik %in% c(node2, node1))
    
    spring_12 =vector()
    for (c in 1:length( unique(node1.node2.table$cusip6) )  ){
    company= unique( node1.node2.table$cusip6  )[c]
    subset = filter(node1.node2.table, cusip6 == company)
    s1 = filter(subset, cik == node1)$value.mln
    s2 =  filter(subset, cik == node2)$value.mln
    if(length(s2) == 0 | length(s1) == 0){spring_12[c]=0}else{
    A2 = filter(subset, cik == node2)$total.value.mln
    C=unique(subset$market.value.mln)
    denominator_12 <-  C*s2+A2*s1
    spring_12[c] = s1*s2/denominator_12}
    
    }
  
    spring_con <- sum(spring_12)
    out_spring <- cbind(node1, node2, spring_con)
     spring_table <- rbind(spring_table, out_spring) 
  }
  
  activist.network <- rbind(activist.network, networks_table)
  activist.network.spring <- rbind(activist.network.spring, spring_table)
  
}
  }

save(activist.network.spring.2015, file="activist.network.spring.2015")
save(simple.network.2015, file="simple.network.2015")

# 