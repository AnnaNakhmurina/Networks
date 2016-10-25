rm(list=ls())

setwd("~/Networks/Analysis")
load("ganch_shark_merged")

# Add classification

load("reduced_campaign")

ganch_shark_merged = merge(ganch_shark_merged, reduced_campaign, by="campaign_id")

load("13f_2000_2007_cik") # file name is cik_13f


ganch_shark_merged <- ganch_shark_merged[rowSums(is.na(ganch_shark_merged))<ncol(ganch_shark_merged),]

# We are only going to need filings that concern a list of given companies as a first step
cusip.list <- unique(ganch_shark_merged$cusip6)
activist.list <- unique( c(ganch_shark_merged$cik, ganch_shark_merged$activist_cik) )
activist.list = activist.list[ which( !is.na(activist.list) )  ]

same.cusips_13f <- subset(cik_13f , cusip6 %in% cusip.list )
# rm(list=setdiff(ls(), c("cusip_ok.dt","ganch_shark_merged")) )

short.data <- data.frame()

for (i in 1: length(cusip.list) ){
print(i)
  
cusip6 <- cusip.list[i]

subsh <- ganch_shark_merged[ganch_shark_merged$cusip6 == cusip6, ]
subsh <- subsh[rowSums(is.na(subsh))<ncol(subsh),]

if(nrow(subsh)>0){
for( campaign in 1:(length(unique(subsh$campaign_id)))  ) {

campaign.id = unique(subsh$campaign_id)[campaign]
  
subsh.camp <- subsh[subsh$campaign_id == campaign.id, ]
subsh.camp <- subsh.camp[rowSums(is.na(subsh.camp))<ncol(subsh.camp),]

active.activist.list <-  unique( c( subsh.camp$cik, subsh.camp$activist_cik  )   )
active.activist.list = active.activist.list[ which( !is.na(active.activist.list) )  ]

passive.activist.list <- setdiff(activist.list, active.activist.list)
passive.activist.list = passive.activist.list[ which( !is.na(passive.activist.list) )  ]

start <- unique(subsh.camp$announce_date)
# end <- unique(subsh.camp$end_date)

# # Use the network structure for the beginning quarter! do not bother about ending quarter by far

sub <- same.cusips_13f[same.cusips_13f$cusip6 == cusip6 ,]
year <- sort( unique(sub$date) )
beginning.quarter <- year[ findInterval(start, year) + 1 ]
# ending.quarter <- year[ findInterval(end, year) + 1 ]
# if ( is.na(ending.quarter) ){  ending.quarter = max(year)  }

# if( !is.na(beginning.quarter) & !is.na(ending.quarter) ){
if( !is.na(beginning.quarter) ){
# sub <- sub[ sub$date >= beginning.quarter & sub$date <= ending.quarter,  ]
sub <- sub[ sub$date == beginning.quarter ,  ]
investor.number <- length( unique(sub$cik) )

activist.set <- sub[sub$cik %in% passive.activist.list,]
activist.number <- length( unique(activist.set$cik) )
  
active.activist.set <- sub[sub$cik %in% active.activist.list,]

# if ( nrow(activist.set) >0 & nrow(active.activist.set) >0 ){
if ( nrow(activist.set) >0 & nrow(active.activist.set) >0 ){
  

#   # Convert to mln every size value
# activist.set$value  = as.numeric( activist.set$value )/1000
# activist.set$total.activ.inv <- sum(  activist.set$value  )
# activist.set$act.weight <-  activist.set$value / activist.set$total.activ.inv
# total.activist.number <- length( unique(activist.set$cik) )
# total.activist.size <- sum( activist.set$total.value )/1000
# activist.size.vweighted <- sum( activist.set$act.weight*activist.set$total.value/1000 )
# print(activist.size.vweighted)
# activist.size.average <- mean(activist.set$total.value)/1000
# active.activist.size <- sum( unique(active.activist.set$total.value) )/1000
# 
# 
# # Alo intoduce betweenness, closeness and bonachich centrality for rach of the networks 
# # Choose to characterize the centrality of a group as a sum of members centrality
# 
# act_simple_closeness <- sum(  active.activist.set$simple_clos  )
# act_simple_betweennes <- sum(  active.activist.set$simple_between  )
# act_simple_bonacich <- sum(  active.activist.set$simple_bonacich  )
# act_spring_closeness <- sum(  active.activist.set$spring_clos  )
# act_spring_betweennes <- sum(  active.activist.set$spring_between  )
# act_spring_bonacich <- sum(  active.activist.set$spring_bonacich  )
# 
# # Add cumulative centrality of the other activist investors in the company
# oth_simple_closeness <- sum(  activist.set$simple_clos  )
# oth_simple_betweennes <- sum(  activist.set$simple_between  )
# oth_simple_bonacich <- sum(  activist.set$simple_bonacich  )
# oth_spring_closeness <- sum(  activist.set$spring_clos  )
# oth_spring_betweennes <- sum(  activist.set$spring_between  )
# oth_spring_bonacich <- sum(  activist.set$spring_bonacich  )
# 
# outcome <- unique( subsh.camp$dissident_board_seats_won )
# 
# if (  outcome %in% c(0) ){won_brep_dummy = 0}else{ won_brep_dummy = 1 }
# 
# won_brep_percent <- unique( subsh.camp$dissident_board_seats_won )/unique( subsh.camp$dissident_board_seats_sought )
# 
# outcome.cl = unique( subsh.camp$success_objective_1 )
# 
# # if (outcome.cl %in% c("Success", "Partial") ){success_of_stated_obj=1}else{success_of_stated_obj=0}
# 
# success_objective_1=subsh.camp$success_objective_1
# success_objective_2=subsh.camp$success_objective_2
# success_objective_3=subsh.camp$success_objective_3
  
# iss_supports = subsh.camp$iss_supports
# glass_lewis_supports = subsh.camp$glass_lewis_supports

output <- data.frame(campaign.id, cusip6, investor.number) 
# , 
                     # total.activist.number, won_brep_dummy, won_brep_percent, 
                     # total.activist.size,activist.size.vweighted,
                     # success_of_stated_obj, active.activist.size, 
                     # activist.size.average,beginning.quarter,ending.quarter, success_objective_1, success_objective_2, success_objective_3,
                     # iss_supports,glass_lewis_supports,
                     # act_simple_closeness, act_simple_betweennes, act_simple_bonacich,
                     # act_spring_closeness, act_spring_betweennes, act_spring_bonacich,
                     # oth_simple_closeness, oth_simple_betweennes, oth_simple_bonacich,
                     # oth_spring_closeness, oth_spring_betweennes, oth_spring_bonacich)
short.data <- rbind(short.data, output)

}

}
  
}
}
}

#-----------------------look how many campaigns are there in gantchev so that activist appears in 13F at least once
# ------ There are 337 such campaigns!!!!

load("13f_2000_2013_cik") 
load("ganch_shark_merged")

potential = data.frame()

for ( campaign_id in unique(ganch_shark_merged$campaign_id) ){
  print(campaign_id)
  
  subg <- ganch_shark_merged[  which(ganch_shark_merged$campaign_id == campaign_id )  ,]
  activists <- unique ( c(subg$cik, subg$activist_cik) )
  activists <- activists [!is.na(activists)]

  
  test = activists %in% cik_13f$cik
  if (  T %in% test  ) {
    potential = rbind( potential, campaign_id  )
  }
  
}

save(potential, file="potential_obs_ganch")

# how many of those are in clen.shark.final?
# There are 1457 such campaigns in the whole database, i.e ~ 1000 additional observations!!!

potential = data.frame()

for ( campaign_id in unique(clean.shark.final$campaign_id) ){
  print(campaign_id)
  
  subg <- clean.shark.final[  which(clean.shark.final$campaign_id == campaign_id )  ,]
  activists <- unique ( c(subg$cik, subg$activist_cik) )
  activists <- activists [!is.na(activists)]
  
  
  test = activists %in% cik_13f$cik
  if (  T %in% test  ) {
    potential = rbind( potential, campaign_id  )
  }
  
}

save(potential, file="potential_obs_shark")

# Compile a file for Saule

load("potential_obs_shark")
load("short.data.classified")
load("short.data.compust")

class_list = unique(c( short.data$campaign.id, short.data.compust$campaign.id ) )
todo_list = setdiff(potential[,1], class_list)

setwd("~/Networks/Analysis")
load("clean.shark.final")
shark =clean.shark.final[(clean.shark.final$announce_date < "2015-01-01"),]

todo = shark [which(shark$campaign_id %in% todo_list),]
todo = todo[c("company_name", "campaign_id", "outcome", "synopsis_text",
              "announce_date", "dissident_group","cusip_9_digit")]

todo=unique(todo)

write.csv( todo, file="to_classify_Saule.csv"  )


#-------------check the total number of potential observations --- 309+100=~400; to classify 309! + 30 (or 52, counting mgrno) from gantchev haha
rm(list=ls())

setwd("~/Networks/Analysis")
load("clean.shark.final")
shark =clean.shark.final[(clean.shark.final$announce_date < "2015-01-01"),]
shark$cusip6 <- substr(shark$cusip_9_digit, 1,6)

load("ganch_shark_merged")

actives = unique(c(ganch_shark_merged$cik, ganch_shark_merged$activist_cik))

cik_13f= rbind(cik_13f_1,cik_13f_2)
save(cik_13f, file="13f_2000_2013_cik")

a = actives[which(actives %in% cik_13f$cik)]

load("13f_2000_2013_cik")
# We are only going to need filings that concern a list of given companies as a first step
cusip.list <- unique(shark$cusip6)
activist.list <- unique( shark$cik )
activist.list = activist.list[ which( !is.na(activist.list) )  ]

same.cusips_13f <- subset(cik_13f , cusip6 %in% cusip.list )
# rm(list=setdiff(ls(), c("cusip_ok.dt","ganch_shark_merged")) )

short.data <- data.frame()

for (i in 1: length(cusip.list) ){
  print(i)
  
  cusip6 <- cusip.list[i]
  
  subsh <- shark[shark$cusip6 == cusip6, ]
  subsh <- subsh[rowSums(is.na(subsh))<ncol(subsh),]
  
  if(nrow(subsh)>0){
    for( campaign in 1:(length(unique(subsh$campaign_id)))  ) {
      
      campaign.id = unique(subsh$campaign_id)[campaign]
      
      subsh.camp <- subsh[subsh$campaign_id == campaign.id, ]
      subsh.camp <- subsh.camp[rowSums(is.na(subsh.camp))<ncol(subsh.camp),]
      
      active.activist.list <-  unique( c( subsh.camp$cik, subsh.camp$activist_cik  )   )
      active.activist.list = active.activist.list[ which( !is.na(active.activist.list) )  ]
      
      passive.activist.list <- setdiff(activist.list, active.activist.list)
      passive.activist.list = passive.activist.list[ which( !is.na(passive.activist.list) )  ]
      
      start <- unique(subsh.camp$announce_date)
      # end <- unique(subsh.camp$end_date)
      
      # # Use the network structure for the beginning quarter! do not bother about ending quarter by far
      
      sub <- same.cusips_13f[same.cusips_13f$cusip6 == cusip6 ,]
      year <- sort( unique(sub$date) )
      beginning.quarter <- year[ findInterval(start, year) + 1 ]
      # ending.quarter <- year[ findInterval(end, year) + 1 ]
      # if ( is.na(ending.quarter) ){  ending.quarter = max(year)  }
      
      # if( !is.na(beginning.quarter) & !is.na(ending.quarter) ){
      if( !is.na(beginning.quarter) ){
        # sub <- sub[ sub$date >= beginning.quarter & sub$date <= ending.quarter,  ]
        sub <- sub[ sub$date == beginning.quarter ,  ]
        investor.number <- length( unique(sub$cik) )
        
        activist.set <- sub[sub$cik %in% passive.activist.list,]
        activist.number <- length( unique(activist.set$cik) )
        
        active.activist.set <- sub[sub$cik %in% active.activist.list,]
        
        if ( nrow(activist.set) >0 & nrow(active.activist.set) >0 ){
          
          #   # Convert to mln every size value
          # activist.set$value  = as.numeric( activist.set$value )/1000
          # activist.set$total.activ.inv <- sum(  activist.set$value  )
          # activist.set$act.weight <-  activist.set$value / activist.set$total.activ.inv
          # total.activist.number <- length( unique(activist.set$cik) )
          # total.activist.size <- sum( activist.set$total.value )/1000
          # activist.size.vweighted <- sum( activist.set$act.weight*activist.set$total.value/1000 )
          # print(activist.size.vweighted)
          # activist.size.average <- mean(activist.set$total.value)/1000
          # active.activist.size <- sum( unique(active.activist.set$total.value) )/1000
          # 
          # 
          # # Alo intoduce betweenness, closeness and bonachich centrality for rach of the networks 
          # # Choose to characterize the centrality of a group as a sum of members centrality
          # 
          # act_simple_closeness <- sum(  active.activist.set$simple_clos  )
          # act_simple_betweennes <- sum(  active.activist.set$simple_between  )
          # act_simple_bonacich <- sum(  active.activist.set$simple_bonacich  )
          # act_spring_closeness <- sum(  active.activist.set$spring_clos  )
          # act_spring_betweennes <- sum(  active.activist.set$spring_between  )
          # act_spring_bonacich <- sum(  active.activist.set$spring_bonacich  )
          # 
          # # Add cumulative centrality of the other activist investors in the company
          # oth_simple_closeness <- sum(  activist.set$simple_clos  )
          # oth_simple_betweennes <- sum(  activist.set$simple_between  )
          # oth_simple_bonacich <- sum(  activist.set$simple_bonacich  )
          # oth_spring_closeness <- sum(  activist.set$spring_clos  )
          # oth_spring_betweennes <- sum(  activist.set$spring_between  )
          # oth_spring_bonacich <- sum(  activist.set$spring_bonacich  )
          # 
          # outcome <- unique( subsh.camp$dissident_board_seats_won )
          # 
          # if (  outcome %in% c(0) ){won_brep_dummy = 0}else{ won_brep_dummy = 1 }
          # 
          # won_brep_percent <- unique( subsh.camp$dissident_board_seats_won )/unique( subsh.camp$dissident_board_seats_sought )
          # 
          # outcome.cl = unique( subsh.camp$success_objective_1 )
          # 
          # # if (outcome.cl %in% c("Success", "Partial") ){success_of_stated_obj=1}else{success_of_stated_obj=0}
          # 
          # success_objective_1=subsh.camp$success_objective_1
          # success_objective_2=subsh.camp$success_objective_2
          # success_objective_3=subsh.camp$success_objective_3
          
          # iss_supports = subsh.camp$iss_supports
          # glass_lewis_supports = subsh.camp$glass_lewis_supports
          
          output <- data.frame(campaign.id, cusip6, investor.number) 
          # , 
          # total.activist.number, won_brep_dummy, won_brep_percent, 
          # total.activist.size,activist.size.vweighted,
          # success_of_stated_obj, active.activist.size, 
          # activist.size.average,beginning.quarter,ending.quarter, success_objective_1, success_objective_2, success_objective_3,
          # iss_supports,glass_lewis_supports,
          # act_simple_closeness, act_simple_betweennes, act_simple_bonacich,
          # act_spring_closeness, act_spring_betweennes, act_spring_bonacich,
          # oth_simple_closeness, oth_simple_betweennes, oth_simple_bonacich,
          # oth_spring_closeness, oth_spring_betweennes, oth_spring_bonacich)
          short.data <- rbind(short.data, output)
          
        }
        
      }
      
    }
  }
}

to_class_merged = short.data
# try to add the classified guys from gantchev

toclas = ganch_shark_merged[c("campaign_id", "success_of_stated_obj")]

to_class_merged = merge(to_class_merged, toclas, by.x="campaign.id", by.y="campaign_id", all.x = T)
to_class_merged = unique(to_class_merged)

short_shark = clean.shark.final[c("company_name", "campaign_id", "outcome", "synopsis_text",
                                  "announce_date", "dissident_group","end_date", "cusip_9_digit")]

to_class_merged = merge(to_class_merged, short_shark, by.x="campaign.id", by.y="campaign_id", all.x = T)
to_class_merged = unique(to_class_merged)
write.csv(to_class_merged, file="to_classify_merged_all.csv")

# calculate the number of appearances

nr.of.appearances <- aggregate(x = ganch_shark_merged,by = list(unique.activists = ganch_shark_merged$cik), FUN = length)

#--------------aggregate funds by mgrno and look how many of the observations could be achieved this way

activist.list <- unique( c(ganch_shark_merged$cik, ganch_shark_merged$activist_cik) )
activist.list = activist.list[ which( !is.na(activist.list) )  ]

same.cusips_13f <- subset(cik_13f , cusip6 %in% cusip.list )

# load mgrno-cik match list 
load("match_cik_13f_wrds_final")

short.data <- data.frame()

for (i in 1: length(cusip.list) ){
  print(i)
  
  cusip6 <- cusip.list[i]
  
  subsh <- ganch_shark_merged[ganch_shark_merged$cusip6 == cusip6, ]
  subsh <- subsh[rowSums(is.na(subsh))<ncol(subsh),]
  
  if(  nrow(subsh)>0  ){for( campaign in 1:(length(unique(subsh$campaign_id)))  ){
      campaign.id = unique(subsh$campaign_id)[campaign]
      
      subsh.camp <- subsh[subsh$campaign_id == campaign.id, ]
      subsh.camp <- subsh.camp[rowSums(is.na(subsh.camp))<ncol(subsh.camp),]
      
      active.activist.list <-  unique( c( subsh.camp$cik, subsh.camp$activist_cik  )   )
      active.activist.list = active.activist.list[ which( !is.na(active.activist.list) )  ]
      active_mgrno = match_cik_13f_wrds_final$mgrno[ which(match_cik_13f_wrds_final$cik  %in% active.activist.list )  ] 
      
      if(length(active_mgrno) > 0){print("FOUND!!!")}
      
      passive.activist.list <- setdiff(activist.list, active.activist.list)
      passive.activist.list = passive.activist.list[ which( !is.na(passive.activist.list) )  ]
      passive_mgrno = match_cik_13f_wrds_final$mgrno[ which(match_cik_13f_wrds_final$cik  %in% passive.activist.list )  ]
      
      start <- unique(subsh.camp$announce_date)
      # end <- unique(subsh.camp$end_date)
      
      # # Use the network structure for the beginning quarter! do not bother about ending quarter by far
      
      sub <- same.cusips_13f[ same.cusips_13f$cusip6 == cusip6 ,]
      year <- sort( unique(sub$date) )
      beginning.quarter <- year[ findInterval(start, year) + 1 ]

      if( !is.na(beginning.quarter) ){
      
        sub <- sub[ sub$date == beginning.quarter ,  ]
        investor.number <- length( unique(sub$cik) )
        inv_number_mgrno = length( unique(sub$mgrno.x) )
        
        activist.set_mgrno <- sub[sub$mgrno.x %in% passive_mgrno,]
        activist.number <- length( unique(activist.set_mgrno$mgrno.x) )
        
        active.activist.set_mgrno <- sub[sub$mgrno.x %in% active_mgrno,]
        
        
        # if ( nrow(activist.set_mgrno) >0 & nrow(active.activist.set_mgrno) >0 ){
        
        if (  nrow(active.activist.set_mgrno) >0 ){
        
          output <- data.frame(campaign.id, cusip6, investor.number) 

          short.data <- rbind(short.data, output)
          
        }
        
      }
      
    }
  }
}

# -----look at the industry specialization of activists: they do not look specialized!

spec = data.frame()

for ( cik in unique(ganch_shark_merged$cik)    ){
  
  a=ganch_shark_merged[ which(ganch_shark_merged$cik == cik), ]
  factset_industry= paste( list( unique(a$factset_industry) ) )
  factset_sector = paste( list( unique(a$factset_sector) ) )
  primary_sic_code_description = paste( list( unique(a$primary_sic_code_description) )  )  
  nr = nrow(a)
  nr_fs_industries = length( unique(a$factset_industry) )
  nr_fs_sectors =  length( unique(a$factset_sector) )
  nr_sic = length( unique(a$primary_sic_code)  )
  nr_sic2 = length( unique( substring(as.character(a$primary_sic_code), 1, 2) ) )
  
  out = as.data.frame( cbind ( cik, factset_industry, nr_fs_industries, factset_sector, nr_fs_sectors, primary_sic_code_description, nr_sic,nr_sic2, nr ) )
  spec = rbind(spec, out)
}





