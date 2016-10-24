rm(list=ls())
setwd("~/Networks")
load("clean.shark.partial")

to.classify = clean.shark.partial[c("company_name", "campaign_id",
                                    "dissident_tactic", "outcome", 
                                    "announce_date", "dissident_group")]
to.classify = unique(to.classify)
to.classify.2015 =to.classify[(to.classify$announce_date> "2014-12-31"),]
write.csv(to.classify.2015, file="to.classify.2015.csv")

# # Select the subsample of data that corresponds to all campaigns that were going of after 
# # the start of 2015
# shark.sub.2015 = clean.shark.partial[!is.na(clean.shark.partial$dissident_board_seats_sought),]
# shark.sub.2015 = shark.sub.2015[(shark.sub.2015$announce_date> "2014-12-31"),]
# 
# out <- shark.sub.2015[c("activist.name","company_name","board_seats_up", "dissident_board_seats_sought",
#         "dissident_board_seats_won", "dissident_tactic_nominate_slate_of_directors", "outcome", "campaign_status" )]
# 
# # Create a 6-digit cusip


classified <- read.csv("classified.2015.csv")
classified$announce_date <- as.Date(classified$announce_date, "%m/%d/%Y")
classified.2015 <- classified[(classified$announce_date< "2016-1-1"),]
c.2015 <- classified.2015[c("campaign_id","activist_objective_1","success_objective_1",
                            "Board.seats.granted.if.objective.is.not.disclosed","Board.seats.up",
                            "activist_objective_2","success_objective_2","activist_objective_3",
                            "success_objective_3")]
data = merge(c.2015, clean.shark.partial, by="campaign_id")

shark.sub.2015 <- data[which(data$activist_objective_1 != 
                               "General undervaluation/maximize shareholder value"),]

shark.sub.2015$cusip6 <- substr(shark.sub.2015$cusip_9_digit, 1,6)
shark.sub.2015 <- shark.sub.2015[rowSums(is.na(shark.sub.2015))<ncol(shark.sub.2015),]
# Put an activists flag 

shark.sub.2015$active <- "active"


# Add classification

load("reduced_campaign")

shark.sub.2015 = merge(shark.sub.2015, reduced_campaign, by="campaign_id")

save(shark.sub.2015, file="shark.sub.2015")

# Now, download the 13f file
setwd("~/Networks")

load("cusip_ok.short")

cusip_ok.short <- data.frame(cusip_ok.short)
cusip_ok.short <- cusip_ok.short[rowSums(is.na(cusip_ok.short))<ncol(cusip_ok.short),]

shark.sub.2015 <- shark.sub.2015[rowSums(is.na(shark.sub.2015))<ncol(shark.sub.2015),]

# We are only going to need filings that concern a list of given companies as a first step
cusip.list <- unique(shark.sub.2015$cusip6)
activist.list <- unique(clean.shark.partial$cik)

same.cusips_13f <- subset( cusip_ok.short, cusip6 %in% cusip.list )
# rm(list=setdiff(ls(), c("cusip_ok.dt","shark.sub.2015")) )
same.cusips_13f$date <- as.Date(same.cusips_13f$period, "%m-%d-%Y")

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
        
        if ( nrow(activist.set) >0 & nrow(active.activist.set) >0 ){
          
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
          
          
          # Alo intoduce betweenness, closeness and bonachich centrality for rach of the networks 
          # Choose to characterize the centrality of a group as a sum of members centrality
          
          act_simple_closeness <- sum(  active.activist.set$simple_clos  )
          act_simple_betweennes <- sum(  active.activist.set$simple_between  )
          act_simple_bonacich <- sum(  active.activist.set$simple_bonacich  )
          act_spring_closeness <- sum(  active.activist.set$spring_clos  )
          act_spring_betweennes <- sum(  active.activist.set$spring_between  )
          act_spring_bonacich <- sum(  active.activist.set$spring_bonacich  )
          
          # Add cumulative centrality of the other activist investors in the company
          oth_simple_closeness <- sum(  activist.set$simple_clos  )
          oth_simple_betweennes <- sum(  activist.set$simple_between  )
          oth_simple_bonacich <- sum(  activist.set$simple_bonacich  )
          oth_spring_closeness <- sum(  activist.set$spring_clos  )
          oth_spring_betweennes <- sum(  activist.set$spring_between  )
          oth_spring_bonacich <- sum(  activist.set$spring_bonacich  )
          
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
          
          output <- data.frame(campaign.id, cusip6, investor.number, 
                               total.activist.number, won_brep_dummy, won_brep_percent, 
                               total.activist.size,activist.size.vweighted,
                               success_of_stated_obj, active.activist.size, 
                               activist.size.average,beginning.quarter,ending.quarter, success_objective_1, success_objective_2, success_objective_3,
                               iss_supports,glass_lewis_supports,
                               act_simple_closeness, act_simple_betweennes, act_simple_bonacich,
                               act_spring_closeness, act_spring_betweennes, act_spring_bonacich,
                               oth_simple_closeness, oth_simple_betweennes, oth_simple_bonacich,
                               oth_spring_closeness, oth_spring_betweennes, oth_spring_bonacich)
          short.data <- rbind(short.data, output)
          
        }
        
      }
      
    }
  }
}


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

short.data.compust = unique( merge(short.data.compust, reduced_campaign, by.x="campaign.id", by.y= "campaign_id", all.x=TRUE) )

save(short.data.compust, file="short.data.compust")


