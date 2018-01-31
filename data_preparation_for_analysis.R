
rm(list=ls())

setwd("D:/Dropbox/Activist paper/Analysis")
load("clean.shark.final_age")

# Count number of appearances of the activist


classified <- read.csv("classified.2015.csv")
classified$announce_date <- as.Date(classified$announce_date, "%m/%d/%Y")
classified.2015 <- classified[(classified$announce_date< "2016-1-1"),]
c.2015 <- classified.2015[c("campaign_id","activist_objective_1","success_objective_1",
                            "Board.seats.granted.if.objective.is.not.disclosed","Board.seats.up",
                            "activist_objective_2","success_objective_2","activist_objective_3",
                            "success_objective_3")]
data = merge(c.2015, clean.shark.final, by="campaign_id")

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
load("shark.sub.2015")
load("cusip_ok.short_w_both_centr")

cusip_ok.short <- data.frame(cusip_ok.short)
cusip_ok.short <- cusip_ok.short[rowSums(is.na(cusip_ok.short))<ncol(cusip_ok.short),]

shark.sub.2015 <- shark.sub.2015[rowSums(is.na(shark.sub.2015))<ncol(shark.sub.2015),]

campaign_type_match = read.csv("campaign_type_match.csv")

# We are only going to need filings that concern a list of given companies as a first step
cusip.list <- unique(shark.sub.2015$cusip6)
activist.list <- unique(clean.shark.final$cik)

same.cusips_13f <- subset( cusip_ok.short, cusip6 %in% cusip.list )
# rm(list=setdiff(ls(), c("cusip_ok.dt","shark.sub.2015")) )
same.cusips_13f$date <- as.Date(same.cusips_13f$period, "%m-%d-%Y")

quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)

load("top20_percent_old")
top20_percent = top20_percent[which(!is.na(top20_percent))]

load("top10_percent_old")
top10_percent = top10_percent[which(!is.na(top10_percent))]

load("top5_percent_old")
top5_percent = top5_percent[which(!is.na(top5_percent))]

short.data <- data.frame()

for (i in 1: length(cusip.list) ){
  
  cusip6 <- cusip.list[ i ]
  subsh <- shark.sub.2015[ shark.sub.2015$cusip6 == cusip6, ]
  subsh <- subsh[ rowSums( is.na(subsh)) < ncol(subsh), ]
  
  if(nrow(subsh)>0){
    for( campaign in 1:length( unique( subsh$campaign_id ) )  ) {
      
      campaign.id = unique( subsh$campaign_id )[ campaign ]
      
      subsh.camp <- subsh[subsh$campaign_id == campaign.id, ]
      subsh.camp <- subsh.camp[rowSums(is.na(subsh.camp))<ncol(subsh.camp),]
      age_activist = mean( subsh.camp$age_activist , na.rm=T )
      active.activist.list <-  unique( subsh.camp$cik )
      passive.activist.list <- setdiff( activist.list, active.activist.list )
      
      start <- unique( subsh.camp$announce_date )
      end <- unique( subsh.camp$end_date )
      
      dissident_group_ownership_percent = max(unique(subsh$dissident_group_ownership_percent))
      
      # Find the quarter range in which the campaign fall in
      
      sub <- same.cusips_13f[ same.cusips_13f$cusip6 == cusip6 , ]
      year <- sort( unique( sub$date ) )
      beginning.quarter <- year[ findInterval(start, year) + 1 ]
      ending.quarter <- year[ findInterval(end, year) + 1 ]
      if (is.na(ending.quarter)){ending.quarter = max(year)}
      
      if( !is.na(beginning.quarter) & !is.na(ending.quarter) ){
        
        # if( !is.na(beginning.quarter) ){
        sub <- sub[ sub$date >= beginning.quarter & sub$date <= ending.quarter,  ]
        # sub <- sub[ sub$date == beginning.quarter ,  ]
        investor.number <- length( unique(sub$cik) )
        top20_number = length(unique(sub$cik[which(sub$cik %in% top20_percent)]))
        top10_number = length(unique(sub$cik[which(sub$cik %in% top10_percent)]))
        top5_number = length(unique(sub$cik[which(sub$cik %in% top5_percent)]))
        
        activist.set <- sub[sub$cik %in% passive.activist.list,]
        top20_set = sub[sub$cik %in% top20_percent,]
        active.activist.set <- sub[sub$cik %in% active.activist.list,]
        
        active_percent = sum(active.activist.set$value.mln.all)/sum( sub$value.mln.all )
        
        active.activist.number = length ( unique(active.activist.set$cik)  )
        
        if ( nrow(activist.set) > 0 & nrow(active.activist.set) > 0 ){
          
          # if ( nrow(active.activist.set) >0 ){
          print(i)
          # Make sure all values are in USD mln
          # activist.set$value  = activist.set$prc*activist.set$shares/(1000000)
          activist.set$total.activ.inv <- sum(  activist.set$value  )
          activist.set$act.weight <-  activist.set$value / activist.set$total.activ.inv
          total.activist.number <- length( unique(activist.set$cik) )
          total.activist.share = sum( activist.set$value )
          total.activist.size <- sum( activist.set$total.value )
          activist.size.vweighted <- sum( activist.set$act.weight*activist.set$total.value)
          activist.share.vweighted <- sum( activist.set$act.weight*activist.set$value)
          print(activist.size.vweighted)
          activist.size.average <- mean(activist.set$total.value)
          activist.share.average <- mean(activist.set$value)
          active.activist.size <- sum( unique(active.activist.set$total.value) )
          active.activist.share = sum(active.activist.set$value.mln.all)
          
          ##### HERE
          
          active.activist_sd_weight = max(active.activist.set$sd_weight)
          active.activist_norm_weight = max(active.activist.set$norm_weight)
          
          # Now introduce the sizes weighed by the FUND network
          
          # Load the corresponding network
          period <- substr( beginning.quarter, 6, 10 )
          year <- substr( beginning.quarter, 1, 4 )
          quarter <- corr$quarter[ which( corr$period == period ) ]
          file = paste0("fund_network_", quarter, "_", year)
          load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
          
          # File is going to be called fund_network
          
          fn <- fund_network[ which( fund_network$fund1 %in% active.activist.list ), ]
          
          if( nrow(fn) == 0 ) { 
            period <- substr(ending.quarter, 6, 10)
            year <- substr(ending.quarter, 1, 4)
            quarter <- corr$quarter[which(corr$period == period)]
            file = paste0("fund_network_", quarter, "_", year)
            load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
            
            fn <- fund_network[which(fund_network$fund1 %in% active.activist.list),]
            }
          
          an = merge(activist.set, fn, by.x="cik", by.y="fund2")
          act_num_con = sum(an$num_con)
          act_s = sum(an$s)
          act_size_nw_s <- sum( an$total.value*an$num_con )
          act_size_nw_spr <- sum( an$total.value*an$s )
          act_share_nw_s <- sum( an$value.mln.all*an$num_con )
          act_share_nw_spr <- sum( an$value.mln.all*an$s )
          
          act_w_sd_s = sum( an$sd_weight*an$num_con )
          act_w_sd_spr = sum( an$sd_weight*an$s )
          act_w_norm_s = sum( an$norm_weight*an$num_con )
          act_w_norm_spr = sum( an$norm_weight*an$s )
          
          invn = merge (sub, fn, by.x= "cik", by.y = "fund2")
          inv_size_nw_s <- sum( invn$total.value*invn$num_con )
          inv_size_nw_spr <- sum( invn$total.value*invn$s )
          
          # Now introduce the sizes weighed by the INVESTOR network
          
          file = paste0("investor_network_top20_", quarter, "_", year)
          load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
          invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          
          if( nrow(invn) == 0 ) { 
            period <- substr(ending.quarter, 6, 10)
            year <- substr(ending.quarter, 1, 4)
            quarter <- corr$quarter[which(corr$period == period)]
            file = paste0("investor_network_top20_", quarter, "_", year)
            load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
            invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          }
          
          top20 <- sub[ sub$cik %in% top20_percent,] 
          
          top20n = merge(top20, invn, by.x="cik", by.y="top20_investor")
          top20_num_con = sum(top20n$num_con)
          top20_s = sum(top20n$s)
          top20_share = sum(  top20n$value.mln.all )
          top20_share_nw_s <- sum( top20n$value.mln.all*top20n$num_con )
          top20_share_nw_spr <- sum( top20n$value.mln.all*top20n$s )
          top20_size_nw_s <- sum( top20n$total.value*top20n$num_con )
          top20_size_nw_spr <- sum( top20n$total.value*top20n$s )
          
          top20_w_sd_s = sum( top20n$sd_weight*top20n$num_con )
          top20_w_sd_spr = sum( top20n$sd_weight*top20n$s )
          top20_w_norm_s = sum( top20n$norm_weight*top20n$num_con )
          top20_w_norm_spr = sum( top20n$norm_weight*top20n$s )
          
          
          
          file = paste0("investor_network_top10_", quarter, "_", year)
          load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
          invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          
          if( nrow(invn) == 0 ) { 
            period <- substr(ending.quarter, 6, 10)
            year <- substr(ending.quarter, 1, 4)
            quarter <- corr$quarter[which(corr$period == period)]
            file = paste0("investor_network_top10_", quarter, "_", year)
            load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
            invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          }
          
          top10 <- sub[ sub$cik %in% top10_percent, ]
          
          top10n = merge(top10, invn, by.x="cik", by.y="top10_investor")
          top10_num_con = sum(top10n$num_con)
          top10_s = sum(top10n$s)
          top10_share = sum(  top10n$value.mln.all )
          top10_share_nw_s <- sum( top10n$value.mln.all*top10n$num_con )
          top10_share_nw_spr <- sum( top10n$value.mln.all*top10n$s )
          top10_size_nw_s <- sum( top10n$total.value*top10n$num_con )
          top10_size_nw_spr <- sum( top10n$total.value*top10n$s )
          
          top10_w_sd_s = sum( top10n$sd_weight*top10n$num_con )
          top10_w_sd_spr = sum( top10n$sd_weight*top10n$s )
          top10_w_norm_s = sum( top10n$norm_weight*top10n$num_con )
          top10_w_norm_spr = sum( top10n$norm_weight*top10n$s )
          
          
          file = paste0("investor_network_top5_new_", quarter, "_", year)
          load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
          invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          
          if( nrow(invn) == 0 ) { 
            period <- substr(ending.quarter, 6, 10)
            year <- substr(ending.quarter, 1, 4)
            quarter <- corr$quarter[which(corr$period == period)]
            file = paste0("investor_network_top5_new_", quarter, "_", year)
            load(paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
            invn <- investor_network[which(investor_network$activist %in% active.activist.list),]
          }
          
          top5 <- sub[sub$cik %in% top5_percent,]
          
          top5n = merge(top5, invn, by.x="cik", by.y="top5_investor")
          top5_num_con = sum(top5n$num_con)
          top5_s = sum(top5n$s)
          top5_share = sum(  top5n$value.mln.all )
          top5_share_nw_s <- sum( top5n$value.mln.all*top5n$num_con )
          top5_share_nw_spr <- sum( top5n$value.mln.all*top5n$s )
          top5_size_nw_s <- sum( top5n$total.value*top5n$num_con )
          top5_size_nw_spr <- sum( top5n$total.value*top5n$s )
          
          top5_w_sd_s = sum( top5n$sd_weight*top5n$num_con )
          top5_w_sd_spr = sum( top5n$sd_weight*top5n$s )
          top5_w_norm_s = sum( top5n$norm_weight*top5n$num_con )
          top5_w_norm_spr = sum( top5n$norm_weight*top5n$s )
          
          top5_perc = sum(  top5n$value.mln.all )/sum(sub$value.mln.all)
          top5_perc_nw_s <- sum( top5n$value.mln.all*top5n$num_con )/sum(sub$value.mln.all)
          top5_perc_nw_spr <- sum( top5n$value.mln.all*top5n$s )/sum(sub$value.mln.all)
          top5_perc_nw_s <- sum( top5n$total.value*top5n$num_con )/sum(sub$value.mln.all)
          top5_perc_nw_spr <- sum( top5n$total.value*top5n$s )/sum(sub$value.mln.all)
          
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
          
          top20_s_clos_inv <- sum(  top20n$inv_simple_clos  )
          top20_s_betw_inv <- sum(  top20n$inv_simple_between  )
          top20_s_bon_inv <- sum(  top20n$inv_simple_bonacich  )
          top20_sp_clos_inv <- sum(  top20n$inv_spring_clos  )
          top20_sp_betw_inv <- sum(  top20n$inv_spring_between  )
          top20_sp_bon_inv <- sum(  top20n$inv_spring_bonacich  )
          
          top10_s_clos_inv <- sum(  top10n$inv_simple_clos  )
          top10_s_betw_inv <- sum(  top10n$inv_simple_between  )
          top10_s_bon_inv <- sum(  top10n$inv_simple_bonacich  )
          top10_sp_clos_inv <- sum(  top10n$inv_spring_clos  )
          top10_sp_betw_inv <- sum(  top10n$inv_spring_between  )
          top10_sp_bon_inv <- sum(  top10n$inv_spring_bonacich  )
          
          top5_s_clos_inv <- sum(  top5n$inv_simple_clos  )
          top5_s_betw_inv <- sum(  top5n$inv_simple_between  )
          top5_s_bon_inv <- sum(  top5n$inv_simple_bonacich  )
          top5_sp_clos_inv <- sum(  top5n$inv_spring_clos  )
          top5_sp_betw_inv <- sum(  top5n$inv_spring_between  )
          top5_sp_bon_inv <- sum(  top5n$inv_spring_bonacich  )
          
          
          # top20_s_clos_inv_real <- sum(  top20n$inv_simple_clos_inv  )
          # top20_s_betw_inv_real <- sum(  top20n$inv_simple_between_inv  )
          # top20_s_bon_inv_real <- sum(  top20n$inv_simple_bonacich_inv  )
          # top20_sp_clos_inv_real <- sum(  top20n$inv_spring_clos_inv  )
          # top20_sp_betw_inv_real <- sum(  top20n$inv_spring_between_inv  )
          # top20_sp_bon_inv_real <- sum(  top20n$inv_spring_bonacich_inv  )
          # 
          # top10_s_clos_inv_real <- sum(  top10n$inv_simple_clos_inv  )
          # top10_s_betw_inv_real <- sum(  top10n$inv_simple_between_inv  )
          # top10_s_bon_inv_real <- sum(  top10n$inv_simple_bonacich_inv  )
          # top10_sp_clos_inv_real <- sum(  top10n$inv_spring_clos_inv  )
          # top10_sp_betw_inv_real <- sum(  top10n$inv_spring_between_inv  )
          # top10_sp_bon_inv_real <- sum(  top10n$inv_spring_bonacich_inv  )
          # 
          # top5_s_clos_inv_real <- sum(  top5n$inv_simple_clos_inv  )
          # top5_s_betw_inv_real <- sum(  top5n$inv_simple_between_inv  )
          # top5_s_bon_inv_real <- sum(  top5n$inv_simple_bonacich_inv  )
          # top5_sp_clos_inv_real <- sum(  top5n$inv_spring_clos_inv  )
          # top5_sp_betw_inv_real <- sum(  top5n$inv_spring_between_inv  )
          # top5_sp_bon_inv_real <- sum(  top5n$inv_spring_bonacich_inv  )
          # 
          outcome <- unique( subsh.camp$dissident_board_seats_won )
          
          if (  outcome %in% c(0) ){won_brep_dummy = 0}else{ won_brep_dummy = 1 }
          
          won_brep_percent <- unique( subsh.camp$dissident_board_seats_won )/unique( subsh.camp$dissident_board_seats_sought )
          
          success_objective_1=subsh.camp$success_objective_1
          success_objective_2=subsh.camp$success_objective_2
          success_objective_3=subsh.camp$success_objective_3
          
          sucfail = c(success_objective_1,success_objective_2,success_objective_3)
          condition = sucfail %in% c("Success", "Partial")
          if ( T %in% condition  ){success_of_stated_obj=1}else{success_of_stated_obj=0}
          
          success_objective_1 = as.numeric(success_objective_1)

          iss_supports = subsh.camp$iss_supports
          glass_lewis_supports = subsh.camp$glass_lewis_supports
          
          if(unique(subsh.camp$poison_pill_in_force_prior_to_announcement) =="Yes"|
             unique(subsh.camp$poison_pill_adopted_in_response_to_campaign =="Yes") ){
            poison_pill = 1 }else{poison_pill = 0}
          
          stock_exchange_primary = unique( subsh.camp$stock_exchange_primary )
          holder_type = unique( subsh.camp$holder_type )
          
          # activist_appearance_number = subsh.camp$activist_appearance_number

          activist_objective_1 = factor( subsh.camp$activist_objective_1, levels = levels( campaign_type_match$objective ) )
          activist_obj_type_1 = campaign_type_match$category[which(campaign_type_match$objective == activist_objective_1)]
          # activist_objective_2 = factor( subsh.camp$activist_objective_2, levels = levels( campaign_type_match$objective ) )
          # activist_obj_type_2 = campaign_type_match$category[which(campaign_type_match$objective == activist_objective_2)]
          # if( length(activist_obj_type_2 ) == 0) { activist_obj_type_2 = NA}
          # activist_objective_3 = factor( subsh.camp$activist_objective_3, levels = levels( campaign_type_match$objective ) )
          # activist_obj_type_3 = unique( campaign_type_match$category[which(campaign_type_match$objective == activist_objective_3)] )
          # if( length(activist_obj_type_3) == 0) {activist_obj_type_3 = NA}
          # 
          output <- data.frame(campaign.id, cusip6, age_activist, 
                               investor.number,active.activist.number, 
                               total.activist.number, won_brep_dummy, won_brep_percent,
                               total.activist.size,activist.size.vweighted,
                               success_of_stated_obj,  active.activist.size,
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
                               top20_size_nw_s, top20_size_nw_spr, top20_number,
                               total.activist.share, active.activist.share,
                               act_num_con, act_s, act_share_nw_s, act_share_nw_spr, top20_num_con,top20_s,
                               top20_share_nw_s,top20_share_nw_spr,
                               top20_s_clos_inv , top20_s_betw_inv , top20_s_bon_inv,  top20_sp_clos_inv,
                               top20_sp_betw_inv,top20_sp_bon_inv,
                               poison_pill, stock_exchange_primary, holder_type,
                               activist.share.average, activist.share.vweighted,
                               active.activist_sd_weight, active.activist_norm_weight,
                               act_w_sd_s, act_w_sd_spr,act_w_norm_s, act_w_norm_spr, 
                               top20_w_sd_s, top20_w_sd_spr, top20_w_norm_s,
                               top20_w_norm_spr , top20_share,
                               top10_size_nw_s, top10_size_nw_spr, top10_number,top10_num_con,top10_s,
                               top10_share_nw_s,top10_share_nw_spr,
                               top10_s_clos_inv , top10_s_betw_inv , top10_s_bon_inv, top10_sp_clos_inv,
                               top10_sp_betw_inv,top10_sp_bon_inv,
                               top10_w_sd_s, top10_w_sd_spr, top10_w_norm_s,
                               top10_w_norm_spr, top10_share,
                               top5_size_nw_s, top5_size_nw_spr, top5_number,top5_num_con,top5_s,
                               top5_share_nw_s,top5_share_nw_spr,
                               top5_s_clos_inv , top5_s_betw_inv , top5_s_bon_inv, top5_sp_clos_inv,
                               top5_sp_betw_inv,top5_sp_bon_inv,
                               top5_w_sd_s, top5_w_sd_spr, top5_w_norm_s,
                               top5_w_norm_spr, top5_share,
                               top5_perc,top5_perc_nw_s , top5_perc_nw_spr,
                               top5_perc_nw_s ,top5_perc_nw_spr, 
                               active_percent, dissident_group_ownership_percent
                               # ,top20_s_clos_inv_real, top20_s_betw_inv_real, top20_s_bon_inv_real,  
                               # top20_sp_clos_inv_real, top20_sp_betw_inv_real, top20_sp_bon_inv_real,
                               # top10_s_clos_inv_real, top10_s_betw_inv_real, top10_s_bon_inv_real,  
                               # top10_sp_clos_inv_real, top10_sp_betw_inv_real, top10_sp_bon_inv_real,
                               # top5_s_clos_inv_real, top5_s_betw_inv_real, top5_s_bon_inv_real,  
                               # top5_sp_clos_inv_real, top5_sp_betw_inv_real, top5_sp_bon_inv_real
                               # ,activist_appearance_number
                               # ,activist_objective_1
                               , activist_obj_type_1
                               # ,activist_objective_2, activist_obj_type_2,
                               # activist_objective_3, activist_obj_type_3
                               )
          short.data <- rbind(short.data, output)
          
        }
        
      }
      
    }
  }
}

# Load compustat

load("compustat_short_2015_w_age")

current = sort( unique( same.cusips_13f$date[! is.na(same.cusips_13f$date)] ) )

compustat_nearestperiod <- aggregate( compustat[c("period")], 
                                      compustat[c("cusip6","date")], 
                                      FUN=function(x) { return( min(current[x<=current]) ) })

compustat_nearestperiod <- compustat_nearestperiod[complete.cases(compustat_nearestperiod),]
names(compustat_nearestperiod) = c("cusip6", "date", "quarter.period")

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

# load("short.data.compust")
# save(short.data.compust, file="short.data.compust_old")
