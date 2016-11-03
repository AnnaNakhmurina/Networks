#This file reads and combines all the manually classified data including the gantchev's data

rm(list=ls())
gc()
setwd("~/Networks/Analysis")

load("clean.shark.final_age")

classified <- read.csv("to_classify_merged_all_classified.csv")
load("ganch_shark_merged")
convert= read.csv("ganch_convert_demand.csv")

m = merge(classified,ganch_shark_merged, by="campaign_id", all.x=T )
m = m[c("success_of_stated_obj.x", "success_of_stated_obj.y","announce_date.x","campaign_id","activist_objective_1","success_objective_1",
        "Board.seats.granted.if.objective.is.not.disclosed","Board.seats.up",
        "activist_objective_2","success_objective_2","activist_objective_3",
        "success_objective_3", "specific_demand_1", "specific_demand_2")]

class_convert = data.frame(stringsAsFactors=F)

for ( i in 1:nrow(m)){
  
  t = m[i,]
  classif = t$success_of_stated_obj.x
  
  if ( classif %in% c(0,1) ){
    
    d1=t[c("specific_demand_1")]
    if(!is.na(d1)){
      o1 = merge(d1, convert, by.x="specific_demand_1", by.y="specific_demand")
      t$activist_objective_1 = o1$objective
      if(  classif == 1 ){
        t$success_objective_1 = "Success"
      }else{t$success_objective_1 = "Failure"}
    }
    
    
    d2=t[c("specific_demand_2")]
    if(!is.na(d2)){
      o2 = merge(d2, convert, by.x="specific_demand_2", by.y="specific_demand")
      t$activist_objective_2 = o2$objective
      if(  classif == 1 ){
        t$success_objective_2 = "Success"
      }else{t$success_objective_2 = "Failure"}
    }
  }
  
  out=as.data.frame(t, stringsAsFactors=F)
  class_convert = rbind(class_convert, out)
  
  
}  

# 
# a = class_convert[which(class_convert$success_of_stated_obj.x %in% c(0,1)),]
# b =a[c("success_of_stated_obj.x","campaign_id","activist_objective_1","success_objective_1",
#        "activist_objective_2","success_objective_2","activist_objective_3",
#        "success_objective_3", "specific_demand_1", "specific_demand_2")]

class_convert$announce_date <- as.Date(class_convert$announce_date.x, "%m/%d/%Y")
c.all <- class_convert[c("campaign_id","activist_objective_1","success_objective_1",
                         "Board.seats.granted.if.objective.is.not.disclosed","Board.seats.up",
                         "activist_objective_2","success_objective_2","activist_objective_3",
                         "success_objective_3")]
data_all = merge(c.all, clean.shark.final, by="campaign_id")

shark.sub.all <- data_all[which(data_all$activist_objective_1 != "General undervaluation/maximize shareholder value"),]

shark.sub.all$cusip6 <- substr(shark.sub.all$cusip_9_digit, 1,6)
shark.sub.all <- shark.sub.all[rowSums(is.na(shark.sub.all))<ncol(shark.sub.all),]
shark.sub.all=unique(shark.sub.all)


# Add classification

load("reduced_campaign")

# shark.sub.2015 = merge(shark.sub.2015, reduced_campaign, by="campaign_id")
shark.sub.all = merge(shark.sub.all, reduced_campaign, by="campaign_id")

save(shark.sub.all, file="shark.sub.all")
# save(shark.sub.2015, file="shark.sub.2015")

#-----------------------Add 13F data to it!
load("shark.sub.all")
load("clean.shark.final_age")
load("13f_2000_2014_w_both_centr")


# We are only going to need filings that concern a list of given companies as a first step
cusip.list <- unique(shark.sub.all$cusip6)
activist.list <- unique(clean.shark.final$cik)

same.cusips_13f <- subset( cik_13f, cusip6 %in% cusip.list )
rm(cik_13f)
same.cusips_13f <- same.cusips_13f[rowSums(is.na(same.cusips_13f))<ncol(same.cusips_13f),]

quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)

load("top20_percent")
top20_percent = top20_percent[which(!is.na(top20_percent))]

short.data <- data.frame()

for (i in 1: length(cusip.list) ){
  
  cusip6 <- cusip.list[i]
  subsh <- shark.sub.all[shark.sub.all$cusip6 == cusip6, ]
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
      
      age_activist = subsh.camp$age_activist
      
      # Find the quarter range in which the campaign fall in
      
      sub <- same.cusips_13f[same.cusips_13f$cusip6 == cusip6 ,]
      year <- sort( unique(sub$date) )
      beginning.quarter <- year[ findInterval(start, year) + 1 ]
      ending.quarter <- year[ findInterval(end, year) + 1 ]
      if (is.na(ending.quarter)){ending.quarter = max(year)}
      
      if( !is.na(beginning.quarter) & !is.na(ending.quarter) ){
        # if( !is.na(beginning.quarter) ){
        sub <- sub[ sub$date >= beginning.quarter & sub$date <= ending.quarter,  ]
        # sub <- sub[ sub$date == beginning.quarter ,  ]
        investor.number <- length( unique(sub$cik) )
        activist.set <- sub[sub$cik %in% passive.activist.list,]
        
        active.activist.set <- sub[sub$cik %in% active.activist.list,]
        
        active.activist.number = length ( unique(active.activist.set$cik)  )
        
        if ( nrow(activist.set) >0 & nrow(active.activist.set) >0 ){
          
          print(i)
          
          # if ( nrow(active.activist.set) >0 ){
          
          # Make sure all values are in USD mln
          # activist.set$value  = activist.set$prc*activist.set$shares/(1000000)
          activist.set$total.activ.inv <- sum(  activist.set$value  )
          activist.set$act.weight <-  activist.set$value / activist.set$total.activ.inv
          total.activist.number <- length( unique(activist.set$cik) )
          total.activist.size <- sum( activist.set$total.value )
          activist.size.vweighted <- sum( activist.set$act.weight*activist.set$total.value)
          print(activist.size.vweighted)
          activist.size.average <- mean(activist.set$total.value)
          active.activist.size <- sum( unique(active.activist.set$total.value) )
          
          # Now introduce the sizes weighed by FUND the network
          
          # Load the corresponding network
          period <- substr(beginning.quarter, 6, 10)
          year <- substr(beginning.quarter, 1, 4)
          quarter <- corr$quarter[which(corr$period == period)]
          file = paste0("fund_network_", quarter, "_", year)
          load(paste0("C:/Users/anakhmur/Documents/Networks/Analysis/networks/", file))
          
          # File is going to be called fund_network
          
          fn <- fund_network[which(fund_network$fund1 %in% active.activist.list),]
          
          if( nrow(fn) == 0 ) { 
            period <- substr(ending.quarter, 6, 10)
            year <- substr(ending.quarter, 1, 4)
            quarter <- corr$quarter[which(corr$period == period)]
            file = paste0("fund_network_", quarter, "_", year)
            load(paste0("C:/Users/anakhmur/Documents/Networks/Analysis/networks/", file))
            
            fn <- fund_network[which(fund_network$fund1 %in% active.activist.list),]
          }
          
          an = merge(activist.set, fn, by.x="cik", by.y="fund2", allow.cartesian=TRUE)
          an = unique(an)
          act_size_nw_s <- sum( an$total.value*an$num_con )
          act_size_nw_spr <- sum( an$total.value*an$s )
          invn = merge (sub, fn, by.x= "cik", by.y = "fund2", allow.cartesian=TRUE)
          invn = unique(invn)
          inv_size_nw_s <- sum( invn$total.value*invn$num_con )
          inv_size_nw_spr <- sum( invn$total.value*invn$s )
          
          
          # Now introduce the sizes weighed by the INVESTOR network
          
          file = paste0("investor_network_", quarter, "_", year)
          load(paste0("C:/Users/anakhmur/Documents/Networks/Analysis/networks/", file))
          invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          
          if( nrow(invn) == 0 ) { 
            period <- substr(ending.quarter, 6, 10)
            year <- substr(ending.quarter, 1, 4)
            quarter <- corr$quarter[which(corr$period == period)]
            file = paste0("investor_network_", quarter, "_", year)
            load(paste0("C:/Users/anakhmur/Documents/Networks/Analysis/networks/", file))
            invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          }
          
          top20 <- sub[sub$cik %in% top20_percent,]
          
          top20n = merge(top20, invn, by.x="cik", by.y="top20_investor", allow.cartesian=TRUE)
          top20n = unique(top20n)
          top20_size_nw_s <- sum( top20n$total.value*top20n$num_con )
          top20_size_nw_spr <- sum( top20n$total.value*top20n$s )
          
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
          
          # Also intoduce betweenness, closeness and bonachich centrality based on INVESTOR of the networks 
          # Choose to characterize the centrality of a group as a sum of members centrality
          act_s_clos_inv <- sum(  active.activist.set$inv_simple_clos  )
          act_s_betw_inv <- sum(  active.activist.set$inv_simple_between  )
          act_s_bon_inv <- sum(  active.activist.set$inv_simple_bonacich  )
          act_sp_clos_inv <- sum(  active.activist.set$inv_spring_clos  )
          act_sp_betw_inv <- sum(  active.activist.set$inv_spring_between  )
          act_sp_bon_inv <- sum(  active.activist.set$inv_spring_bonacich  )
          
          # Add cumulative centrality of the other activist investors in the company
          oth_s_clos_inv <- sum(  activist.set$inv_simple_clos  )
          oth_s_betw_inv <- sum(  activist.set$inv_simple_between  )
          oth_s_bon_inv <- sum(  activist.set$inv_simple_bonacich  )
          oth_sp_clos_inv <- sum(  activist.set$inv_spring_clos  )
          oth_sp_betw_inv <- sum(  activist.set$inv_spring_between  )
          oth_sp_bon_inv <- sum(  activist.set$inv_spring_bonacich  )
          
          outcome <- unique( subsh.camp$dissident_board_seats_won )
          
          if (  outcome %in% c(0) ){won_brep_dummy = 0}else{ won_brep_dummy = 1 }
          
          won_brep_percent <- unique( subsh.camp$dissident_board_seats_won )/unique( subsh.camp$dissident_board_seats_sought )
          
          success_objective_1 = as.character( subsh.camp$success_objective_1 )
          success_objective_2 = as.character( subsh.camp$success_objective_2 )
          success_objective_3 = as.character( subsh.camp$success_objective_3 )
          
          sucfail = c(success_objective_1,success_objective_2,success_objective_3)
          condition = sucfail %in% c("Success", "Partial")
          if ( T %in% condition  ){success_of_stated_obj=1}else{success_of_stated_obj=0}
          
          iss_supports = subsh.camp$iss_supports
          glass_lewis_supports = subsh.camp$glass_lewis_supports
          
          output <- data.frame(campaign.id, cusip6, age_activist, 
                               investor.number,active.activist.number, 
                               total.activist.number, won_brep_dummy, won_brep_percent,
                               total.activist.size,activist.size.vweighted,
                               success_of_stated_obj, active.activist.size,
                               activist.size.average,beginning.quarter,ending.quarter, success_objective_1, success_objective_2, success_objective_3,
                               iss_supports,glass_lewis_supports,
                               act_s_clos, act_s_betw, act_s_bon, act_sp_clos, 
                               act_sp_betw, act_sp_bon,
                               oth_s_clos,oth_s_betw, oth_s_bon, oth_sp_clos, 
                               oth_sp_betw, oth_sp_bon,
                               act_s_clos_inv, act_s_betw_inv, act_s_bon_inv, act_sp_clos_inv, 
                               act_sp_betw_inv, act_sp_bon_inv,
                               oth_s_clos_inv, oth_s_betw_inv, oth_s_bon_inv, oth_sp_clos_inv, 
                               oth_sp_betw_inv, oth_sp_bon_inv,
                               act_size_nw_s, act_size_nw_spr,inv_size_nw_s,inv_size_nw_spr,
                               top20_size_nw_s, top20_size_nw_spr)
          short.data <- rbind(short.data, output)
          
        }
        
      }
      
      
    }
  }
  
}

# sh = short.data

save(short.data, file="short.data.classified")

# ----------- Add COMPUSTAT to the short data
load("13f_2000_2014_w_both_centr")
load("short.data.classified")
load("compustat_short_2000_2014")

current = sort( unique( same.cusips_13f$date[! is.na(same.cusips_13f$date)] ) )

compustat_nearestperiod <- aggregate( compustat[c("period")], 
                                      compustat[c("cusip6","date")], 
                                      FUN=function(x) { return( min(current[x<=current]) ) })

compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
names(compustat_nearestperiod) = c("cusip6", "date", "quarter.period")

compustat.short <- compustat[c("cusip6", "datadate","date","age", "leverage", "size",
                               "mtb","oper_profit",  "roa", "tobins_q", 
                               "asset_turnover", "rd_to_assets" ,"revtq")]

compustat.short = merge(compustat.short,compustat_nearestperiod, by=c("date", "cusip6"))

compustat.end.period <- compustat.short[c("cusip6", "quarter.period", "revtq",
                                          "oper_profit")]

names(compustat.end.period) <- c("cusip6", "quarter.period", "revtq_end","oper_profit_end")
# Bind compustat and short.data

short.data.compust <- merge(short.data, compustat.short, by.x=c("beginning.quarter", "cusip6"), 
                            by.y=c("quarter.period", "cusip6"), all.x=TRUE)
short.data.compust <- merge(short.data.compust, compustat.end.period, by.x=c("ending.quarter", "cusip6"),
                            by.y=c("quarter.period", "cusip6"), all.x=TRUE)


short.data.compust$sales_growth = (short.data.compust$revtq_end - short.data.compust$revtq)/short.data.compust$revtq
short.data.compust$oper_profit_growth = (short.data.compust$oper_profit_end - short.data.compust$oper_profit)/short.data.compust$oper_profit

load("reduced_campaign")

short.data.compust = unique( merge(short.data.compust, reduced_campaign, by.x="campaign.id", by.y= "campaign_id", all.x=TRUE) )

save(short.data.compust, file="short.data.compust_2000_2014")

#----------------Nice! Now combine 200-2014 data and 2015 data
rm(list=ls())
setwd("~/Networks/Analysis")
library(plyr)

load("short.data.compust_2000_2014")

# Add campaign characteristics
load("shark.sub.all")
char = shark.sub.all[c("campaign_id","activist_objective_1",
                       "activist_objective_2","activist_objective_3")]

short.data.compust_2000_2014 = merge(short.data.compust, char, by.x="campaign.id", by.y="campaign_id")
short.data.compust_2000_2014 = unique(short.data.compust_2000_2014)

load("short.data.compust")
load("shark.sub.2015")

char.15 = shark.sub.2015[c("campaign_id","activist_objective_1",
                           "activist_objective_2","activist_objective_3")]

short.data.compust_2015 = merge(short.data.compust, char.15, by.x="campaign.id", by.y="campaign_id")
short.data.compust_2015 = unique(short.data.compust_2015)

sh = rbind.fill(short.data.compust_2000_2014, short.data.compust_2015)

exit_s_board <- vector()

for (i in 1:nrow(sh)){
  s = sh$exit_stage[i]
  if(s == "board_repres" ){exit_s_board[i] = 1}else{exit_s_board[i] = 0}
  }
  
sh$exit_s_board = exit_s_board

exit_s_proxy <- vector()

for (i in 1:nrow(sh)){
  s = sh$exit_stage[i]
  if(s == "proxy_fight" ){exit_s_proxy[i] = 1}else{exit_s_proxy[i] = 0}
}

sh$exit_s_proxy = exit_s_proxy

short.data.compust = sh


# Rescale all the centralities to 0:1

short.data.compust$act_s_bon = scale(short.data.compust$act_s_bon)
short.data.compust$act_s_clos = scale(short.data.compust$act_s_clos)
short.data.compust$act_s_betw = scale(short.data.compust$act_s_betw )
short.data.compust$act_sp_bon = scale(short.data.compust$act_sp_bon )
short.data.compust$act_sp_clos = scale(short.data.compust$act_sp_clos )
short.data.compust$act_sp_betw = scale(short.data.compust$act_sp_betw )
short.data.compust$oth_s_bon = scale(short.data.compust$oth_s_bon )
short.data.compust$oth_s_clos = scale(short.data.compust$oth_s_clos )
short.data.compust$oth_s_betw = scale(short.data.compust$oth_s_betw )
short.data.compust$oth_sp_bon = scale(short.data.compust$oth_sp_bon )
short.data.compust$oth_sp_clos = scale(short.data.compust$oth_sp_clos )
short.data.compust$oth_sp_betw = scale(short.data.compust$oth_sp_betw )


save(short.data.compust, file="short.data.compust_2000_2015")

# 
# # load("short.data.compust_2000_2015")
# camp_list = short.data.compust$campaign.id
# load("clean.shark.final")
# 
# to_coll = clean.shark.final[which(clean.shark.final$campaign_id %in% camp_list),]
# to_coll = unique(to_coll)
# activists_age = to_coll[c("activist.name", "cik")]
# activists_age = unique(activists_age)
# write.csv(activists_age, file="activists_age.csv")
# board_seats = to_coll[c("dissident_board_seats_won", "dissident_board_seats_sought", "campaign_id", "synopsis_text", "board_seats_up")]
# board_seats= unique(board_seats)
# write.csv(board_seats, file="board_seats.csv")

load("short.data.compust_2000_2015")
board = read.csv("board_seats_checked.csv")
board = board[c("campaign.id","checked_board_seats_sought", "checked_board_seats_won", "checked_board_seats_up"  )]

short.data.compust = merge(short.data.compust, board, by="campaign.id", all.x=T)

won_board_ind <- vector()

for (i in 1:nrow(short.data.compust)){
  s = short.data.compust$checked_board_seats_won[i]
  if (!is.na(s)){
    if(s == 0 ){won_board_ind[i] = 0}else{won_board_ind[i] = 1}
  }else{won_board_ind[i] = NA}
}

short.data.compust$won_board_ind = won_board_ind

save(short.data.compust, file="short.data.compust_2000_2015")

# load("short.data.compust")
# save(short.data.compust, file="short.data.compust_2000_2015_old_network")








