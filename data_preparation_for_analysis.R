rm(list=ls())
# gc()
setwd("~/Networks/Analysis")
load("clean.shark.final")

classified <- read.csv("classified.2015.csv")
classified$announce_date <- as.Date(classified$announce_date, "%m/%d/%Y")
classified.2015 <- classified[(classified$announce_date< "2016-1-1"),]
c.2015 <- classified.2015[c("campaign_id","activist_objective_1","success_objective_1",
                            "Board.seats.granted.if.objective.is.not.disclosed","Board.seats.up",
                            "activist_objective_2","success_objective_2","activist_objective_3",
                            "success_objective_3")]
shark.sub.2015 = merge(c.2015, clean.shark.final, by="campaign_id")

shark.sub.2015 <- data[which(data$activist_objective_1 !=
                                 "General undervaluation/maximize shareholder value"),]

shark.sub.2015$cusip6 <- substr(shark.sub.2015$cusip_9_digit, 1,6)
shark.sub.2015 <- shark.sub.2015[rowSums(is.na(shark.sub.2015))<ncol(shark.sub.2015),]
# Put an activists flag 

# shark.sub.2015$active <- "active"

# Add classification

load("reduced_campaign")

shark.sub.2015 = merge(shark.sub.2015, reduced_campaign, by="campaign_id")
save(shark.sub.2015, file="shark.sub.2015")

# Now, download the 13f file

load("13f_2000_2015_w_centralities")

cusip_ok.short = cik_13f
cusip_ok.short <- data.frame(cusip_ok.short)
cusip_ok.short <- cusip_ok.short[rowSums(is.na(cusip_ok.short))<ncol(cusip_ok.short),]
shark.sub.2015 <- shark.sub.2015[rowSums(is.na(shark.sub.2015))<ncol(shark.sub.2015),]

# We are only going to need filings that concern a list of given companies as a first step
cusip.list <- unique(shark.sub.2015$cusip6)
activist.list <- unique(clean.shark.final$cik)
same.cusips_13f <- subset( cusip_ok.short, cusip6 %in% cusip.list )
# rm(list=setdiff(ls(), c("cusip_ok.dt","shark.sub.2015")) )

nw_files = list.files(path = "C:/Users/anakhmur/Documents/Networks/Analysis/networks", pattern = "fund_network")

# Create corespondence table 
quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)
short.data <- data.frame()

for (i in 1: length(cusip.list) ){

cusip6 <- cusip.list[i]
subsh <- shark.sub.2015[shark.sub.2015$cusip6 == cusip6, ]
subsh <- subsh[rowSums(is.na(subsh))<ncol(subsh),]

if(nrow(subsh)>0){
for( campaign in 1:(length(unique(subsh$campaign_id)))  ) {

campaign.id = unique(subsh$campaign_id)[campaign]
  
subsh.camp <- subsh[subsh$campaign_id == campaign.id, ]
subsh.camp <- subsh.camp[rowSums(is.na(subsh.camp))<ncol(subsh.camp),]

active.activist.list <-  unique(subsh.camp$cik)
passive.activist.list <- setdiff(activist.list, active.activist.list)

start <- unique(subsh.camp$announce_date)
end <- unique(subsh.camp$end_date)

# Find the quarter range in which the campaign fall in

sub <- same.cusips_13f[same.cusips_13f$cusip6 == cusip6 ,]
year <- sort( unique(sub$date) )
lag.quarter <-  year[ findInterval(start, year) ]
beginning.quarter <- year[ findInterval(start, year) + 1 ]
ending.quarter <- year[ findInterval(end, year) + 1 ]
if (is.na(ending.quarter)){ending.quarter = max(year)}

sub_lag = sub[ sub$date == lag.quarter ,  ]
lag_activist.set <- sub_lag[sub_lag$cik %in% passive.activist.list,]
lag_act_number <- length ( unique(lag_activist.set$cik)  )


if( !is.na(beginning.quarter) & !is.na(ending.quarter) ){
# if( !is.na(beginning.quarter) ){
sub <- sub[ sub$date >= beginning.quarter & sub$date <= ending.quarter,  ]
# sub <- sub[ sub$date == beginning.quarter ,  ]
investor.number <- length( unique(sub$cik) )
activist.set <- sub[sub$cik %in% passive.activist.list,]

active.activist.set <- sub[sub$cik %in% active.activist.list,]
active.activist.number = length ( unique(active.activist.set$cik)  )

if ( nrow(activist.set) >0 & nrow(active.activist.set) >0 ){
  # print(i)

# if ( nrow(active.activist.set) >0 ){
  
  # Convert to mln every size value
activist.set$value  = as.numeric( activist.set$value )/1000
activist.set$total.activ.inv <- sum(  activist.set$value  )
activist.set$act.weight <-  activist.set$value / activist.set$total.activ.inv
total.activist.number <- length( unique(activist.set$cik) )
total.activist.size <- sum( activist.set$total.value )/1000
activist.size.vweighted <- sum( activist.set$act.weight*activist.set$total.value/1000 )
print(activist.size.vweighted)
activist.size.average <- mean(activist.set$total.value)/1000
active.activist.size <- sum( unique(active.activist.set$total.value) )/1000

#load the network file corresponding to the beginning quarter 
nw_date = substr(beginning.quarter, 6,10)
nw_y = substr(beginning.quarter,1,4)
nw_q = corr$quarter[which(corr$period == nw_date)]
nw_file = paste0("C:/Users/anakhmur/Documents/Networks/Analysis/networks/", "fund_network_", nw_q,"_",nw_y)
load(nw_file)

active_guys <- unique(active.activist.set$cik)
sub_nw = fund_network[which(fund_network$fund1 %in% active_guys),]

if(nrow(sub_nw) == 0){
  nw_date = substr(ending.quarter, 6,10)
  nw_y = substr(ending.quarter,1,4)
  nw_q = corr$quarter[which(corr$period == nw_date)]
  nw_file = paste0("C:/Users/anakhmur/Documents/Networks/Analysis/networks/", "fund_network_", nw_q,"_",nw_y)
  nw_path = paste0 ("")
  load(nw_file)
  
  active_guys <- unique(active.activist.set$cik)
  sub_nw = fund_network[which(fund_network$fund1 %in% active_guys),]
}

# Weight size by the number of connections
sub_nw_inv =sub_nw[which(sub_nw$fund2 %in% unique(sub$cik)),]
sub_nw_inv= merge(sub, sub_nw_inv, by.x="cik", by.y="fund2")
inv_size_nw_s = sub_nw_inv$total.value*sub_nw_inv$num_con/sum(sub_nw_inv$num_con)
inv_size_nw_s = sum( inv_size_nw_s  )
inv_size_nw_spr = sub_nw_inv$total.value*sub_nw_inv$s
inv_size_nw_spr = sum( inv_size_nw_spr  )

sub_nw_activ = sub_nw[which(sub_nw$fund2 %in% activist.set$cik),]
sub_nw_activ = merge(activist.set, sub_nw_activ, by.x="cik", by.y="fund2")
act_size_nw_s = sub_nw_activ$total.value*sub_nw_activ$num_con/sum(sub_nw_activ$num_con)
act_size_nw_s = sum(act_size_nw_s)
act_size_nw_spr = sub_nw_activ$total.value*sub_nw_activ$s
act_size_nw_spr = sum(act_size_nw_spr)


# Also intoduce betweenness, closeness and bonachich centrality for rach of the networks 
# Choose to characterize the centrality of a group as a sum of members centrality
act_s_clos <- sum(  active.activist.set$simple_clos  )
act_s_betw <- sum(  active.activist.set$simple_between  )
act_s_bon <- sum(  active.activist.set$simple_bonacich  )
act_sp_clos <- sum(  active.activist.set$spring_clos  )
act_sp_betw <- sum(  active.activist.set$spring_between  )
act_sp_bon <- sum(  active.activist.set$spring_bonacich  )

# Add cumulative centrality of the other activist investors in the company
oth_s_clos <- sum(  activist.set$simple_clos  )
oth_s_betw <- sum(  activist.set$simple_between  )
oth_s_bon <- sum(  activist.set$simple_bonacich  )
oth_sp_clos <- sum(  activist.set$spring_clos  )
oth_sp_betw <- sum(  activist.set$spring_between  )
oth_sp_bon <- sum(  activist.set$spring_bonacich  )

outcome <- unique( subsh.camp$dissident_board_seats_won )

if (  outcome %in% c(0) ){won_brep_dummy = 0}else{ won_brep_dummy = 1 }

won_brep_percent <- unique( subsh.camp$dissident_board_seats_won )/unique( subsh.camp$dissident_board_seats_sought )

outcome.cl = unique( subsh.camp$success_objective_1 )

if (outcome.cl %in% c("Success", "Partial") ){success_of_stated_obj=1}else{success_of_stated_obj=0}

success_objective_1=subsh.camp$success_objective_1
success_objective_2=subsh.camp$success_objective_2
success_objective_3=subsh.camp$success_objective_3
iss_supports = subsh.camp$iss_supports
glass_lewis_supports = subsh.camp$glass_lewis_supports

output <- data.frame(campaign.id, cusip6, investor.number,active.activist.number, 
                     total.activist.number, won_brep_dummy, won_brep_percent,
                     total.activist.size,activist.size.vweighted,
                     success_of_stated_obj, active.activist.size,
                     activist.size.average,beginning.quarter,ending.quarter, success_objective_1, success_objective_2, success_objective_3,
                     iss_supports,glass_lewis_supports,lag_act_number,
                     act_s_clos, act_s_betw, act_s_bon, act_sp_clos, 
                     act_sp_betw, act_sp_bon,
                     oth_s_clos,oth_s_betw, oth_s_bon, oth_sp_clos, 
                     oth_sp_betw, oth_sp_bon, inv_size_nw_s, inv_size_nw_spr, act_size_nw_s,act_size_nw_spr)
short.data <- rbind(short.data, output)

}

}
  
}
}
}

sh15 = short.data

load("short.data.classified")

sh00_15 = rbind(sh15,short.data)


# Load compustat

load("compustat_q_2015_w_age")

compustat$leverage = (compustat$dlttq+compustat$dlcq)/compustat$seqq

for (i in 1:nrow(compustat)){
  if(!is.na(compustat$mkvaltq[i])){
    compustat$market.value.mln[i] <- compustat$mkvalt[i]
    
  }else{compustat$market.value.mln[i] <- compustat$cshoq[i]*compustat$prccq[i]}
  
}

compustat$size = compustat$market.value.mln
compustat$mtb = compustat$market.value.mln/compustat$ceqq

# If XRD (R&D) expenses are NA, substitute them with 0 
compustat$xrdq[is.na(compustat$xrdq)] <- 0

compustat$oper_profit = compustat$revtq - compustat$cogsq - (compustat$xsgaq - compustat$xrdq)

compustat$roa = compustat$atq/compustat$niq
compustat$tobins_q = (compustat$size+compustat$dlttq+compustat$dlcq +compustat$txditcq)/compustat$atq
compustat$asset_turnover = compustat$saleq/compustat$atq
compustat$rd_to_assets = compustat$xrdq
compustat$period = as.Date(as.character(compustat$datadate),"%Y%m%d")

current = sort( unique( same.cusips_13f$date[! is.na(same.cusips_13f$date)] ) )


compustat_nearestperiod <- aggregate( compustat[c("period")], 
                                      compustat[c("cusip6","date")], 
                                      FUN=function(x) { return( min(current[x<=current]) ) })

compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
names(compustat_nearestperiod) = c("cusip6", "date", "quarter.period")
# Operating profitability: from BLGN sales minus COGS minus SGNA
#To generate the b o ok-to-market ratio, we calculate the b o ok value of equity as shareholders'
#equity, plus balance sheet deferred taxes, plus balance s he et investment tax credits, 
# plus p ostretirement b enefit liabilities , and minus preferred sto ck.

compustat.short <- compustat[c("cusip6", "datadate","date","age", "leverage", "size",
                               "mtb","oper_profit",  "roa", "tobins_q", 
                               "asset_turnover", "rd_to_assets" ,"revtq", "saleq")]

compustat.short = merge(compustat.short,compustat_nearestperiod, by=c("date", "cusip6"))

compustat.end.period <- compustat.short[c("cusip6", "quarter.period", "saleq","revtq",
                                          "oper_profit")]

names(compustat.end.period) <- c("cusip6", "quarter.period", "saleq_end", "revtq_end","oper_profit_end")
# Bind compustat and short.data

short.data.compust <- merge(short.data, compustat.short, by.x=c("beginning.quarter", "cusip6"), 
            by.y=c("quarter.period", "cusip6"), all.x=TRUE)
short.data.compust <- merge(short.data.compust, compustat.end.period, by.x=c("ending.quarter", "cusip6"),
                            by.y=c("quarter.period", "cusip6"), all.x=TRUE)


short.data.compust$sales_growth = (short.data.compust$saleq_end - short.data.compust$saleq)/short.data.compust$saleq
short.data.compust$oper_profit_growth = (short.data.compust$oper_profit_end - short.data.compust$oper_profit)/short.data.compust$oper_profit

load("reduced_campaign")

short.data.compust = ( merge(short.data.compust, reduced_campaign, by.x="campaign.id", by.y= "campaign_id", all.x=TRUE) )

save(short.data.compust, file="short.data.compust")
