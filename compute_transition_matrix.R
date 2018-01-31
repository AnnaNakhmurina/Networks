rm(list=ls())
gc()
setwd("D:/Dropbox/Activist paper/Analysis")

library(plyr)
library(qdap)
library(multiwayvcov)
library(lmtest)


load("13f_2000_2015_w_both_centr")

load("shark.sub.2015")
load("shark.sub.all")

load("clean.shark.final_age")
full_activist_list <- unique(clean.shark.final$cik)
full_activist_list <- full_activist_list [which(!is.na(full_activist_list))]

'%!in%' <- function(x,y)!('%in%'(x,y))

quarters.list = sort(unique(cik_13f$date))

all_campaigns = rbind.fill(shark.sub.all,shark.sub.2015)
all_campaigns <- all_campaigns[(all_campaigns$end_date < "2016-1-1"),]

success_of_stated_obj <- vector()

for (i in 1:length(all_campaigns$campaign_id) ){
  
  campaign = all_campaigns$campaign_id[i]
  
  subsh.camp <- all_campaigns[ which( all_campaigns$campaign_id == campaign ), ]
  success_objective_1 = as.character( subsh.camp$success_objective_1 )
  success_objective_2 = as.character( subsh.camp$success_objective_2 )
  success_objective_3 = as.character( subsh.camp$success_objective_3 )
  
  sucfail = c(success_objective_1,success_objective_2,success_objective_3)
  # sucfail = success_objective_1
  condition = sucfail %in% c("Success", "Partial")
  
  if ( T %in% condition  ){ success_of_stated_obj[i]=1 }else{ success_of_stated_obj[i]=0 }
  
}

all_campaigns$success_of_stated_obj = success_of_stated_obj

all_campaigns = all_campaigns[ c("campaign_id","cik","cusip6", "announce_date", "end_date", 
                                 "success_of_stated_obj")  ]

announce = (all_campaigns$announce_date)
start_year <- ( substr(announce, 1, 4))
beginning.quarter <-( quarters.list[ findInterval(announce, quarters.list) + 1 ] )
end =  (all_campaigns$end_date)
ending.quarter <- quarters.list[ findInterval(end, quarters.list) + 1 ]

all_campaigns$beginning.quarter = beginning.quarter
all_campaigns$ending.quarter = ending.quarter

all_campaigns = all_campaigns[which(all_campaigns$announce_date > "2000-1-1"),]

quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)

save(all_campaigns, file="all_campaigns")

#---------------------Compute fot activists--------------

outcomes <- data.frame()

for( i in 1:nrow(all_campaigns) ){
  
  print(i)
  
  campaign = all_campaigns$campaign_id[i]
  campaign_data = all_campaigns[ which( all_campaigns$campaign_id == campaign ), ]
  
  firm_cusip = campaign_data$cusip6
  activist_cik   = campaign_data$cik[!is.na(campaign_data$cik)]
  campaign_outcome = unique( campaign_data$success_of_stated_obj )
  
  
  out <- data.frame()
  
  for(activist in activist_cik){
    
    # load start network files 
    announce = unique(campaign_data$announce_date)
    start_year <- unique( substr(announce, 1, 4))
    if( start_year < 2000 ){  start_year=2000  }
    
    beginning.quarter <-unique( quarters.list[ findInterval(announce, quarters.list) + 1 ] )
    data_13f = cik_13f[ which(cik_13f$date == beginning.quarter & cik_13f$cusip6 == firm_cusip), ]
    connect_list <- setdiff( unique(data_13f$cik), activist )
    
    start_period <- unique( substr(beginning.quarter, 6, 10) )
    quarter <- corr$quarter[which(corr$period == start_period)]
    
    file = paste0("fund_network_", quarter, "_", start_year)
    load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
    
    fn_init <- fund_network[which(fund_network$fund1 %in% activist & fund_network$fund2 %in% connect_list),]
    
    if( nrow(fn_init)>0 ){
      fn_init$date = beginning.quarter
      names(fn_init) = c("activist_cik", "connect_cik", paste0("init_", names(fn_init)[3:5]))
      fn_init$campaign_outcome = campaign_outcome
      fn_init$treated = 1
    }
    
    all_others_init <- fund_network[which(fund_network$fund1 %in% activist & fund_network$fund2 %!in% connect_list),]
    if( nrow(all_others_init)>0 ){
      all_others_init$date = beginning.quarter
      names(all_others_init) = c("activist_cik", "connect_cik", paste0("init_", names(all_others_init)[3:5]))
      all_others_init$campaign_outcome = campaign_outcome
      all_others_init$treated = 0
      fn_init = rbind(fn_init, all_others_init)
    }
    
    # what happens after? 
    end = unique(campaign_data$end_date)
    next_after_end <- quarters.list[ findInterval(end, quarters.list) + 2 ]
    next_q <- substr(next_after_end, 6, 10)
    end_year <- substr(next_after_end, 1, 4)
    
    if ( !is.na(end_year) ){
      if(end_year < 2000){end_year=2000}
      next_quarter <- corr$quarter[which(corr$period == next_q)]
      
      
      file = paste0("fund_network_", next_quarter, "_", end_year)
      load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
      fn_next <- fund_network[which(fund_network$fund1 %in% activist & fund_network$fund2 %!in% connect_list),]
      
      if( nrow(fn_next)>0 ){
        fn_next$date = next_after_end
        names(fn_next) = c("activist_cik","connect_cik", paste0("next_", names(fn_next)[3:5]) )
        fn_next$treated = 1
      }
      
      
      all_others_next <- fund_network[which(fund_network$fund1 %in% activist & fund_network$fund2 %!in% connect_list),]
      
      if(nrow(all_others_next) >0){
        all_others_next$date = next_after_end
        names(all_others_next) = c("activist_cik", "connect_cik", paste0("next_", names(all_others_next)[3:5]))
        all_others_next$treated = 0
        fn_next = rbind(fn_next, all_others_next)
      }
      
      if( nrow(fn_init) >0 & nrow(fn_next) >0){
        o  = merge( fn_init, fn_next, all =T )
        o$campaign_id = campaign
      }else{
        
        if(nrow(fn_init) == 0 & nrow(fn_next) >0){
          
          df = data.frame(matrix(NA, nrow=nrow(fn_next), ncol=6))
          names(df) <- names(outcomes)[1:6]
          df$activist_cik = fn_next$activist_cik
          df$connect_cik = fn_next$connect_cik
          df$init_date = beginning.quarter
          df$campaign_outcome = campaign_outcome
          o  = merge( df, fn_next, all =T )
          o$campaign_id = campaign
        }else{if( nrow(fn_next)==0 & nrow(fn_init) >0 ){
          
          df = data.frame(matrix(NA, nrow=nrow(fn_init), ncol=5))
          names(df) <- names(outcomes)[7:11]
          df$activist_cik = fn_next$activist_cik
          df$connect_cik = fn_next$connect_cik
          df$init_date = next_after_end
          o  = merge( fn_init, df, all =T )
          o$campaign_id = campaign
        }else{ o <-data.frame() }
          
        }
        
      }
      
      o  = NAer(o)
      
      
      out <- rbind(out, o)
      
      
    }
    
    outcomes <- rbind(out, outcomes)
    
  }
  
}

outcomes$next_num_con = as.numeric(as.character(outcomes$next_num_con))
outcomes$init_num_con = as.numeric(as.character(outcomes$init_num_con))
outcomes$next_s = as.numeric(as.character(outcomes$next_s))
outcomes$init_s = as.numeric(as.character(outcomes$init_s))
outcomes$campaign_outcome = as.numeric(as.character(outcomes$campaign_outcome))
outcomes$treated = as.numeric(as.character(outcomes$treated))
#
outcomes$diff_number = outcomes$next_num_con - outcomes$init_num_con 
outcomes$diff_s = outcomes$next_s - outcomes$init_s 

a = outcomes[c("campaign_outcome", "diff_number", "diff_s", "campaign_id")]
b = aggregate( a[c("diff_number", "diff_s")], a[c("campaign_id")], FUN=sum )
c = outcomes[c("campaign_outcome", "campaign_id")]
d = merge(b,c)
d=unique(d)

didreg = lm((scale(diff_s)) ~ campaign_outcome+treated+campaign_outcome*treated, data = outcomes)
re_didreg  = sqrt(diag(  cluster.vcov(didreg, outcomes$campaign_id) ) )
summary(didreg, se=re_didreg)
save(outcomes, file="activist_outcomes")

#------------------------------ Do the same with investor networks---------------------------

outcomes <- data.frame()

for( i in 1:nrow(all_campaigns) ){
  
  print(i)
  
  campaign = all_campaigns$campaign_id[i]
  campaign_data = all_campaigns[ which( all_campaigns$campaign_id == campaign ), ]
  
  firm_cusip = campaign_data$cusip6
  activist_cik   = campaign_data$cik[!is.na(campaign_data$cik)]
  campaign_outcome = unique( campaign_data$success_of_stated_obj )
  
  
  out <- data.frame()
  
  for(activist in activist_cik){
    
    # load start network files 
    announce = unique(campaign_data$announce_date)
    start_year <- unique( substr(announce, 1, 4))
    if(start_year < 2000){start_year=2000}
    
    beginning.quarter <-unique( quarters.list[ findInterval(announce, quarters.list) + 1 ] )
    data_13f = cik_13f[ which(cik_13f$date == beginning.quarter & cik_13f$cusip6 == firm_cusip), ]
    connect_list <- setdiff( unique(data_13f$cik), activist )
    
    start_period <- unique( substr(beginning.quarter, 6, 10) )
    quarter <- corr$quarter[which(corr$period == start_period)]
    
    file = paste0("investor_network_", quarter, "_", start_year)
    load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
    
    fn_init <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %in% connect_list),]
    
    if( nrow(fn_init)>0 ){
      fn_init$date = beginning.quarter
      names(fn_init) = c("activist_cik", "connect_cik", paste0("init_", names(fn_init)[3:5]))
      fn_init$campaign_outcome = campaign_outcome
      fn_init$treated = 1
    }
    
    all_others_init <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %!in% connect_list),]
    if( nrow(all_others_init)>0 ){
      all_others_init$date = beginning.quarter
      names(all_others_init) = c("activist_cik", "connect_cik", paste0("init_", names(all_others_init)[3:5]))
      all_others_init$campaign_outcome = campaign_outcome
      all_others_init$treated = 0
      
      fn_init = rbind(fn_init, all_others_init)
    }
    
    # what happens after? 
    end = unique(campaign_data$end_date)
    next_after_end <- quarters.list[ findInterval(end, quarters.list) + 2 ]
    next_q <- substr(next_after_end, 6, 10)
    end_year <- substr(next_after_end, 1, 4)
    
    if ( !is.na(end_year) ){
      if(end_year < 2000){end_year=2000}
      next_quarter <- corr$quarter[which(corr$period == next_q)]
      
      
      file = paste0("investor_network_", next_quarter, "_", end_year)
      load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
      fn_next <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %in% connect_list),]
      
      if( nrow(fn_next)>0 ){
        fn_next$date = next_after_end
        names(fn_next) = c("activist_cik","connect_cik", paste0("next_", names(fn_next)[3:5]) )
        fn_next$treated = 1
      }
      
      
      all_others_next <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %!in% connect_list),]
      
      if(nrow(all_others_next) >0){
        all_others_next$date = next_after_end
        names(all_others_next) = c("activist_cik", "connect_cik", paste0("next_", names(all_others_next)[3:5]))
        all_others_next$treated = 0
        fn_next = rbind(fn_next, all_others_next)
      }
      
      if( nrow(fn_init) >0 & nrow(fn_next) >0){
        o  = merge( fn_init, fn_next, all =T )
        o$campaign_id = campaign
      }else{
        
        if(nrow(fn_init) == 0 & nrow(fn_next) >0){
          
          df = data.frame(matrix(NA, nrow=nrow(fn_next), ncol=6))
          names(df) <- names(outcomes)[1:6]
          df$activist_cik = fn_next$activist_cik
          df$connect_cik = fn_next$connect_cik
          df$init_date = beginning.quarter
          df$campaign_outcome = campaign_outcome
          o  = merge( df, fn_next, all =T )
          o$campaign_id = campaign
        }else{if( nrow(fn_next)==0 & nrow(fn_init) >0 ){
          
          df = data.frame(matrix(NA, nrow=nrow(fn_init), ncol=5))
          names(df) <- names(outcomes)[7:11]
          df$activist_cik = fn_next$activist_cik
          df$connect_cik = fn_next$connect_cik
          df$init_date = next_after_end
          o  = merge( fn_init, df, all =T )
          o$campaign_id = campaign
        }else{ o <-data.frame() }
          
        }
        
      }
      
      o  = NAer(o)
      
      
      out <- rbind(out, o)
      
      
    }
    
    outcomes <- rbind(out, outcomes)
    
  }
  
}

outcomes$next_num_con = as.numeric(as.character(outcomes$next_num_con))
outcomes$init_num_con = as.numeric(as.character(outcomes$init_num_con))
outcomes$next_s = as.numeric(as.character(outcomes$next_s))
outcomes$init_s = as.numeric(as.character(outcomes$init_s))
outcomes$campaign_outcome = as.numeric(as.character(outcomes$campaign_outcome))
outcomes$treated = as.numeric(as.character(outcomes$treated))
#
outcomes$diff_number = outcomes$next_num_con - outcomes$init_num_con 
outcomes$diff_s = outcomes$next_s - outcomes$init_s 
outcomes$year = as.numeric(substr(outcomes$init_date,1,4))

a = outcomes[c("campaign_outcome", "diff_number", "diff_s", "campaign_id")]
b = aggregate( a[c("diff_number", "diff_s")], a[c("campaign_id")], FUN=sum )
c = outcomes[c("campaign_outcome", "campaign_id")]
d = merge(b,c)
d=unique(d)

didreg = lm( diff_number ~ campaign_outcome+treated+campaign_outcome*treated+year, 
             data = outcomes)
clust = cluster.vcov(didreg, outcomes$campaign_id)
clust = cluster.boot(didreg, outcomes$campaign_id)
re_didreg  = sqrt(diag(  cluster.vcov(didreg, outcomes$campaign_id) ) )
summary(didreg)
coeftest(didreg, clust)
# save(outcomes, file="investor_outcomes")


for(i in 1:length(unique(outcomes$campaign_id))){
  
  print(i)
  
  campaign = outcomes$campaign_id[i]
  
  data <- outcomes[which(outcomes$campaign_id == campaign),]
  
  didreg = lm( diff_s ~ campaign_outcome+treated+campaign_outcome*treated+year, 
               data = data)
  
}

g0 = outcomes[which(outcomes$treated == 0 ),]
group1 = outcomes[which(outcomes$treated == 1 & outcomes$campaign_outcome == 1),]
group2 = outcomes[which(outcomes$treated == 1 & outcomes$campaign_outcome == 0),]


#---------------------Do a DiD that controls for what is happenning with other activists
#---------------------After a successful campaign--------------------------------------

outcomes_t_c <- data.frame()


database_names = c("activist_cik","connect_cik", "init_s", "init_num_con","init_date","campaign_outcome"
                   ,"in_same_company","next_s","next_num_con", "next_date","campaign_id" )
i=7
for(i in 1: length(all_campaigns$campaign_id)){
print(i)

campaign = all_campaigns$campaign_id[i]
campaign_data = all_campaigns[ which( all_campaigns$campaign_id == campaign ), ]

firm_cusip = unique(campaign_data$cusip6)
activist_cik   = campaign_data$cik[!is.na(campaign_data$cik)]
campaign_outcome = unique( campaign_data$success_of_stated_obj )
print(campaign_outcome)
out <- data.frame()

for(activist in activist_cik){
  # print(activist)
  
  # load start network files 
  announce = unique(campaign_data$announce_date)
  start_year <- unique( substr(announce, 1, 4))
  if(start_year > 1999){
  
  beginning.quarter <- unique( quarters.list[ findInterval(announce, quarters.list) + 1 ] )
  data_13f = cik_13f[ which(cik_13f$date == beginning.quarter & cik_13f$cusip6 == firm_cusip), ]
  connect_list <- setdiff( unique(data_13f$cik), activist )
  
  start_period <- unique( substr(beginning.quarter, 6, 10) )
  start_quarter <- corr$quarter[which(corr$period == start_period)]
  
  file = paste0("investor_network_", start_quarter, "_", start_year)
  load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
  
  fn_init <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %in% connect_list),]
  
  if( nrow(fn_init)>0 ){
    fn_init$date = beginning.quarter
    names(fn_init) = c("activist_cik", "connect_cik", paste0("init_", names(fn_init)[3:5]))
    fn_init$campaign_outcome = campaign_outcome
    fn_init$in_same_company = 1
  }
  
  all_others_init <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %!in% connect_list),]
  if( nrow(all_others_init)>0 ){
    all_others_init$date = beginning.quarter
    names(all_others_init) = c("activist_cik", "connect_cik", paste0("init_", names(all_others_init)[3:5]))
    all_others_init$campaign_outcome = campaign_outcome
    all_others_init$in_same_company = 0
    
    fn_init = rbind(fn_init, all_others_init)
  }
  
  # what happens after? 
  end = unique(campaign_data$end_date)
  ending.quarter <- quarters.list[ findInterval(end, quarters.list) + 1 ]
  next_after_end <- quarters.list[ findInterval(end, quarters.list) + 2 ]
  next_q <- substr(next_after_end, 6, 10)
  end_year <- substr(next_after_end, 1, 4)
  
  if ( !is.na(end_year) ){
    if(end_year < 2000){end_year=2000}
    next_quarter <- corr$quarter[which(corr$period == next_q)]
    
    
    file = paste0("investor_network_", next_quarter, "_", end_year)
    load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
    fn_next <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %in% connect_list),]
    
    if( nrow(fn_next)>0 ){
      fn_next$date = next_after_end
      names(fn_next) = c("activist_cik","connect_cik", paste0("next_", names(fn_next)[3:5]) )
      fn_next$in_same_company = 1
    }
    
    all_others_next <- investor_network[which(investor_network$activist %in% activist & investor_network$top20_investor %!in% connect_list),]
    
    if(nrow(all_others_next) >0){
      all_others_next$date = next_after_end
      names(all_others_next) = c("activist_cik", "connect_cik", paste0("next_", names(all_others_next)[3:5]))
      all_others_next$in_same_company = 0
      fn_next = rbind(fn_next, all_others_next)
    }
    
    if( nrow(fn_init) >0 & nrow(fn_next) >0){
      o  = merge( fn_init, fn_next, all =T )
      o$campaign_id = campaign
      o$treated <- 1
      }else{
      
      if(nrow(fn_init) == 0 & nrow(fn_next) >0){
        
        df = data.frame(matrix(NA, nrow=nrow(fn_next), ncol=6))
        names(df) <- database_names[1:6]
        df$activist_cik = fn_next$activist_cik
        df$connect_cik = fn_next$connect_cik
        df$init_date = beginning.quarter
        df$campaign_outcome = campaign_outcome
        o  = merge( df, fn_next, all =T )
        o$campaign_id = campaign
        o$treated <- 1
      }else{if( nrow(fn_next)==0 & nrow(fn_init) >0 ){
                df = data.frame(matrix(NA, nrow=nrow(fn_init), ncol=5))
        names(df) <- database_names[7:11]
        df$activist_cik = fn_init$activist_cik
        df$connect_cik = fn_init$connect_cik
        df$next_date = next_after_end
        o  = merge( fn_init, df, all =T )
        o$campaign_id = campaign
        o$treated <- 1
      }else{ o <-data.frame() }
        
      }
    }
    
    o  = NAer(o)
   
    
    out <- rbind(out, o)
  }
    
  }

    # outcomes <- rbind(out, outcomes)
  
}

# select the list of activists that do not do a campaign during the [beginning.quarter, ending quarter period]
# guys that do a campaign

active_campaigns = clean.shark.final[which(clean.shark.final$announce_date >= beginning.quarter & clean.shark.final$announce_date <= ending.quarter | 
                              clean.shark.final$end_date >= beginning.quarter & clean.shark.final$end_date <= ending.quarter ),]
 
active_activists = active_campaigns$cik

# Guys that do not do a public campaign

not_active_activists = unique( setdiff(full_activist_list, active_activists) )
data_13f_control = cik_13f[ which(cik_13f$date == beginning.quarter & cik_13f$cik %in% not_active_activists), ]
not_active_activists = data_13f$cik[which(data_13f$cik %in% not_active_activists) ]

not_out<- data.frame()

for( not_active in not_active_activists ){
  
  print(not_active)
  if(start_year > 1999){
  file = paste0("investor_network_", start_quarter, "_", start_year)
  load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
  not_init <- investor_network[which(investor_network$activist %in% not_active ),]
  
  if( nrow(not_init)>0 ){
    not_init$date = beginning.quarter
    names(not_init) = c("activist_cik", "connect_cik", paste0("init_", names(not_init)[3:5]))
    not_init$campaign_outcome = 0
    not_init$in_same_company = 0
  }
  
  if ( !is.na(end_year) ){
    
    file = paste0("investor_network_", next_quarter, "_", end_year)
    load(paste0("C:/Users/anakhmur/Dropbox/Activist paper/Analysis/networks/", file))
    not_next <- investor_network[which(investor_network$activist %in% not_active),]
    
    if( nrow(not_next)>0 ){
      not_next$date = next_after_end
      names(not_next) = c("activist_cik","connect_cik", paste0("next_", names(not_next)[3:5]) )
      # not_next$in_same_company = NA
    }
    
    if( nrow(not_init) >0 & nrow(not_next) >0 ){
      o  = merge( not_init, not_next, all =T )
      o$campaign_id = campaign
      o$treated = 0
    }else{if( nrow(not_init) == 0 & nrow(not_next) >0 ){
        df = data.frame(matrix(NA, nrow=nrow(not_next), ncol=6))
        names(df) <- database_names[1:6]
        df$activist_cik = not_next$activist_cik
        df$connect_cik = not_next$connect_cik
        df$init_date = beginning.quarter
        df$campaign_outcome = campaign_outcome
        o  = merge( df, not_next, all =T )
        o$campaign_id = campaign
        o$treated = 0
      }else{if( nrow(not_next)==0 & nrow(not_init) >0 ){
        
        df = data.frame(matrix(NA, nrow=nrow(not_init), ncol=5))
        names(df) <- database_names[7:11]
        df$activist_cik = not_init$activist_cik
        df$connect_cik = not_init$connect_cik
        df$next_date = next_after_end
        o  = merge( not_init, df, all =T )
        o$campaign_id = campaign
        o$treated = 0
      }else{ o <-data.frame() }
        
      }
      
    }
    
    o  = NAer(o)
   
    
    not_out <- rbind(not_out, o)
    
  }
  }
  
}

outcomes = rbind(out, not_out)

outcomes_t_c = rbind(outcomes_t_c, outcomes)

}


outcomes_t_c$next_num_con = as.numeric(as.character(outcomes_t_c$next_num_con))
outcomes_t_c$init_num_con = as.numeric(as.character(outcomes_t_c$init_num_con))
outcomes_t_c$next_s = as.numeric(as.character(outcomes_t_c$next_s))
outcomes_t_c$init_s = as.numeric(as.character(outcomes_t_c$init_s))
outcomes_t_c$campaign_outcome = as.numeric(as.character(outcomes_t_c$campaign_outcome))
outcomes_t_c$treated = as.numeric(as.character(outcomes_t_c$treated))
#
outcomes_t_c$diff_number = outcomes_t_c$next_num_con - outcomes_t_c$init_num_con 
outcomes_t_c$diff_s = outcomes_t_c$next_s - outcomes_t_c$init_s 
outcomes_t_c$year = as.numeric(substr(outcomes_t_c$init_date,1,4))
outcomes_t_c$in_same_company = as.numeric(outcomes_t_c$in_same_company)

save(outcomes_t_c, file="outcomes_t_s_top20")

# t = complete.cases(outcomes_t_c)

didreg = lm( diff_number ~ treated, data = outcomes_t_c)

didreg = lm( diff_number ~ treated+campaign_outcome+treated*campaign_outcome, 
             data = outcomes_t_c )
didreg = lm( winsorResult ~ treated, 
             data = a)

summary(didreg)

library(data.table)
library(psych)
library(dummies)
library(lfe)

load( "outcomes_t_s_top20" )
myDT <- data.table(outcomes_t_c)

a=myDT[, winsorResult := winsor(diff_number, trim = 0.05)]

g0 = outcomes_t_c[which(outcomes_t_c$campaign_outcome ==0),]
g1 = outcomes_t_c[which(outcomes_t_c$campaign_outcome ==1),]

didreg = lm( diff_number ~ treated, 
            data = g1)

didreg = lm( winsorResult ~ treated, 
             data = a)

g1 = outcomes_t_c[which(outcomes_t_c$treated ==1),]
g0 = outcomes_t_c[which(outcomes_t_c$treated ==0),]

hist(g2$diff_number,  breaks = 500,  col="gray")
hist(g1$diff_number, breaks = 300, col="red",add=T)


winning_campaigns = g1$campaign_id[which(g1$campaign_outcome ==1)]
winning_campaigns = unique(winning_campaigns)
losing_campaigns = g1$campaign_id[which(g0$campaign_outcome ==0)]
losing_campaigns = unique(losing_campaigns)


winning = outcomes_t_c[which(outcomes_t_c$campaign_id %in% winning_campaigns),]
winning$win =1
losing = outcomes_t_c[which(outcomes_t_c$campaign_id %in% losing_campaigns),]
losing$win = 0

winlose = rbind(winning, losing)
save(winlose, file= "winlose_top20_control_other")

d = dummy(winning$campaign_id)
a = cbind(winning, d)

load("winlose_top20_control_other")

id_dummy <- factor(winning$campaign_id)

didreg = lm( diff_s ~ treated+factor(campaign_id), 
             data = winning )
didreg_1 = lm( diff_s ~ treated+in_same_company+treated*in_same_company, 
             data = winning )
didreg_2 = lm( diff_s ~ treated+in_same_company+treated*in_same_company+factor(campaign_id), 
               data = winning )
didreg_3 = lm( diff_s ~ treated+in_same_company+treated*in_same_company+factor(campaign_id), 
               data = losing )

did_3 =  lm( diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company , 
             data = winlose )
did_3 =  lm( diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company+factor(campaign_id) , 
             data = winlose )
did_3 =  felm( diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company | campaign_id+year , 
             data = winlose )
summary(did_3)

did_30 = lm(diff_number ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company , data= winlose)

se_30  = sqrt(diag(  cluster.vcov(did_30, winlose$campaign_id) ) )

did_31 = lm(diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company , data= winlose)

se_31  = sqrt(diag(  cluster.vcov(did_31, winlose$campaign_id) ) )

did_out = capture.output(stargazer(  did_30,did_31,
                                       type = "latex",  
                                       title = "Diff-in-diff-in-diff",
                                       font.size = "tiny", float.env="table",header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       report = "vct*",
                                       digits = 4,
                                       # omit = c("holder_type", "age","log(size)", "leverage", "mtb"),
                                       # omit.labels = "Firm specific controls?",
                                       single.row = TRUE,
                                       se=list(se_30, se_31 )))
cat (did_out, sep = "\n")

d = cluster.vcov(did_31, winlose$campaign_id)



# coeftest(did_31, d)
# 
# 
# load("short.data.compust_2000_2015")
# 
# types = data.frame(short.data.compust$campaign.id, short.data.compust$type1)
# 
# wl = merge(winlose, types, by.x="campaign",  by.y ="campaign.id")


summary(didreg_1)
summary(didreg_2)
summary(didreg_3)
summary(didreg)$coefficients[ !grepl("campaign_id", names(coef(didreg)) ) , ,drop=FALSE] 
summary(didreg)$coefficients[ !grepl("campaign_id", names(coef(didreg)) ) , ,drop=FALSE] 


#-------------------Build panel data--------------------------------------------------

load("all_campaigns")

load("13f_2000_2015_w_both_centr")
'%!in%' <- function(x,y)!('%in%'(x,y))

load("clean.shark.final_age")
full_activist_list <- unique(clean.shark.final$cik)
full_activist_list <- full_activist_list [which(!is.na(full_activist_list))]

# Create summary statistics regarding the fund networks
nw_files = list.files(path = "D:/Dropbox/Activist paper/Analysis/networks", pattern = "investor_network_top20")
nw_files = nw_files[1:64]

quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)
quarters.list = sort(unique(cik_13f$date))

fund_n <- data.frame()

for (file in nw_files){
  
  print(file)
  load( paste0("D:/Dropbox/Activist paper/Analysis/networks/", file))
  q = substr(file, 24, 25)
  y = substr(file, 27, 30)
  quat = corr$period[which(corr$quarter == q)]
  date = as.Date( paste0(quat,"-",y), "%m-%d-%Y" )
  investor_network$date = date
  fund_n <- rbind(fund_n, investor_network)
}

dates = sort( unique(fund_n$date) )

campaign_t_c = data.frame()

for( i in 1:length(dates) ){
  
  print(i)
  
  date = dates[i]
  next_date = dates[i+1]

  # network_data = fund_n[ which( fund_n$date == date ), ]
  # next_network_data = fund_n[which(fund_n$date == next_date ),]

  ongoing_campaigns = all_campaigns[which( all_campaigns$beginning.quarter <= date & all_campaigns$ending.quarter >= date),]
  campaign_year = all_campaigns[which(all_campaigns$ending.quarter == date),]


  if( nrow(campaign_year) > 0 ){
    
      print("campaign_found")
      
      campaign = campaign_year$campaign_id
      
      o = data.frame()
      
      for( c in campaign ){
      
      campaign_data = all_campaigns[ which( all_campaigns$campaign_id %in% c ), ]
       
      firm_cusip = unique( campaign_data$cusip6 )
      activist_cik = unique( campaign_data$cik[ !is.na( campaign_data$cik ) ] )
      
      # activist = activist_cik
      
      campaign_outcome = unique( campaign_data$success_of_stated_obj )
      beginning.quarter = campaign_data$beginning.quarter
      ending.quarter = campaign_data$ending.quarter
      data_13f = cik_13f[ which( cik_13f$date == beginning.quarter & cik_13f$cusip6 == firm_cusip ), ]
      connect_list <- setdiff( unique(data_13f$cik), activist_cik )
      
      # Create a current network:
      network_data = fund_n[ which( fund_n$date >= beginning.quarter & fund_n$date <= ending.quarter ), ]
      nw_activist = network_data[ which( network_data$activist == activist_cik ), ]
      
      if( nrow(nw_activist) > 0 ){
        
          nw_activist$campaign_id = c 
          nw_activist$treated = 1
          nw_activist$win = campaign_outcome
          nw_activist$time_end  = ifelse( nw_activist$date == ending.quarter , 1, 0 )
          nw_activist$time_start  = ifelse( nw_activist$date == beginning.quarter , 1, 0 )
          nw_activist$t = 1 
          nw_activist$t_minus_1 = 0
          nw_activist$t_minus_2 = 0
          nw_activist$t_minus_3 = 0
          nw_activist$t_plus_1 = 0
          nw_activist$t_plus_2 = 0
          nw_activist$t_plus_3 = 0
      }
      
      nw_activist_connect = nw_activist[ which(nw_activist$top20_investor %in% connect_list),  ]
     
      if( nrow(nw_activist_connect) >0 ){
         
            nw_activist_connect$in_same_company = 1
      }
      
      nw_activist_not_connect =  nw_activist[ which(nw_activist$top20_investor %!in% connect_list),  ]
      
      if( nrow(nw_activist_not_connect) >0 ){
        
          nw_activist_not_connect$in_same_company = 0
      
            }
      
      nw_activist = rbind( nw_activist_connect, nw_activist_not_connect )
      
      # Create a nw up to 3 periods before:
      
      previous_periods = quarters.list[ (findInterval(beginning.quarter, quarters.list) - 3) : ( findInterval(beginning.quarter, quarters.list) - 1) ]
      previous_network = fund_n[ which( fund_n$date %in%  previous_periods ), ]
      previous_nw_activist = previous_network[ which( previous_network$activist == activist_cik ), ]
      
      if( nrow(previous_nw_activist) > 0 ){
        
        previous_nw_activist$campaign_id = c 
        previous_nw_activist$treated = 1
        previous_nw_activist$win = campaign_outcome
        previous_nw_activist$time_end  = ifelse( previous_nw_activist$date == ending.quarter , 1, 0 )
        previous_nw_activist$time_start  = ifelse( previous_nw_activist$date == beginning.quarter , 1, 0 )
        previous_nw_activist$t = 0
        previous_nw_activist$t_minus_1 = ifelse( previous_nw_activist$date == previous_periods[3] , 1, 0 )
        previous_nw_activist$t_minus_2 = ifelse( previous_nw_activist$date == previous_periods[2] , 1, 0 )
        previous_nw_activist$t_minus_3 = ifelse( previous_nw_activist$date == previous_periods[1] , 1, 0 )
        previous_nw_activist$t_plus_1 = 0
        previous_nw_activist$t_plus_2 = 0
        previous_nw_activist$t_plus_3 = 0
        
      }
      
      previous_nw_activist_connect = previous_nw_activist[ which(previous_nw_activist$top20_investor %in% connect_list),  ]
      
      if( nrow(previous_nw_activist_connect) >0 ){
        
        previous_nw_activist_connect$in_same_company = 1
      }
      
      previous_nw_activist_not_connect =  previous_nw_activist[ which(previous_nw_activist$top20_investor %!in% connect_list),  ]
      
      if( nrow(previous_nw_activist_not_connect) >0 ){
        
        previous_nw_activist_not_connect$in_same_company = 0
        
      }
      
      previous_nw_activist = rbind( previous_nw_activist_connect, previous_nw_activist_not_connect )
      
      # Create a nw up to 3 periods after:
      
      next_periods = quarters.list[ (findInterval(ending.quarter, quarters.list) +1 ) : ( findInterval(beginning.quarter, quarters.list) + 3) ]
      next_network = fund_n[ which( fund_n$date %in%  next_periods ), ]
      next_nw_activist = next_network[ which( next_network$activist == activist_cik ), ]
      
      if( nrow(next_nw_activist) > 0 ){
        
        next_nw_activist$campaign_id = c 
        next_nw_activist$treated = 1
        next_nw_activist$win = campaign_outcome
        next_nw_activist$time_end  = ifelse( next_nw_activist$date == ending.quarter , 1, 0 )
        next_nw_activist$time_start  = ifelse( next_nw_activist$date == beginning.quarter , 1, 0 )
        next_nw_activist$t = 0
        next_nw_activist$t_minus_1 = 0
        next_nw_activist$t_minus_2 = 0
        next_nw_activist$t_minus_3 = 0
        next_nw_activist$t_plus_1 = ifelse( next_nw_activist$date == next_periods[1] , 1, 0 )
        next_nw_activist$t_plus_2 = ifelse( next_nw_activist$date == next_periods[2] , 1, 0 )
        next_nw_activist$t_plus_3 = ifelse( next_nw_activist$date == next_periods[3] , 1, 0 )
        
      }
      
      next_nw_activist_connect = next_nw_activist[ which(next_nw_activist$top20_investor %in% connect_list),  ]
      
      if( nrow(next_nw_activist_connect) >0 ){
        
        next_nw_activist_connect$in_same_company = 1
      }
      
      next_nw_activist_not_connect =  next_nw_activist[ which(next_nw_activist$top20_investor %!in% connect_list),  ]
      
      if( nrow(next_nw_activist_not_connect) >0 ){
        
        next_nw_activist_not_connect$in_same_company = 0
        
      }
      
      next_nw_activist = rbind( next_nw_activist_connect, next_nw_activist_not_connect )
      
      # Gather all the treatment sample:
      
      treatment = rbind( previous_nw_activist, nw_activist )
      treatment = rbind( treatment, next_nw_activist )
      
      # Now, do a control group
      
      active_campaigns = clean.shark.final[which(clean.shark.final$announce_date >= beginning.quarter &
                                                   clean.shark.final$announce_date <= ending.quarter | 
                                                   clean.shark.final$end_date >= beginning.quarter & 
                                                   clean.shark.final$end_date <= ending.quarter ),]
      
      active_activists = active_campaigns$cik
      not_active_activists = unique( setdiff(full_activist_list, active_activists) )
      
      data_13f_control = cik_13f[ which(cik_13f$date == date & cik_13f$cik %in% not_active_activists), ]
      not_active_activists = unique( data_13f_control$cik[which(data_13f_control$cik %in% not_active_activists) ] )
      
      # Current period
      nw_not_active_activist = network_data[which(network_data$activist %in% not_active_activists ),]
     
      if( nrow(nw_not_active_activist) >0 ){
        
      nw_not_active_activist$campaign_id = c
      nw_not_active_activist$treated = 0
      nw_not_active_activist$win = campaign_outcome
      nw_not_active_activist$time_end  = ifelse( nw_not_active_activist$date == ending.quarter , 1, 0 )
      nw_not_active_activist$time_start  = ifelse( nw_not_active_activist$date == beginning.quarter , 1, 0 )
      nw_not_active_activist$t = 1 
      nw_not_active_activist$t_minus_1 = 0
      nw_not_active_activist$t_minus_2 = 0
      nw_not_active_activist$t_minus_3 = 0
      nw_not_active_activist$t_plus_1 = 0
      nw_not_active_activist$t_plus_2 = 0
      nw_not_active_activist$t_plus_3 = 0
      
      nw_not_active_activist$in_same_company = 0
      
      }
      
      # Previous period
      
      previous_not_active_activist = previous_network[ which( previous_network$activist %in% not_active_activists ), ]
      
      if( nrow(previous_not_active_activist) > 0 ){
        
        previous_not_active_activist$campaign_id = c 
        previous_not_active_activist$treated = 0
        previous_not_active_activist$win = campaign_outcome
        previous_not_active_activist$time_end  = ifelse( previous_not_active_activist$date == ending.quarter , 1, 0 )
        previous_not_active_activist$time_start  = ifelse( previous_not_active_activist$date == beginning.quarter , 1, 0 )
        previous_not_active_activist$t = 0
        previous_not_active_activist$t_minus_1 = ifelse( previous_not_active_activist$date == previous_periods[3] , 1, 0 )
        previous_not_active_activist$t_minus_2 = ifelse( previous_not_active_activist$date == previous_periods[2] , 1, 0 )
        previous_not_active_activist$t_minus_3 = ifelse( previous_not_active_activist$date == previous_periods[1] , 1, 0 )
        previous_not_active_activist$t_plus_1 = 0
        previous_not_active_activist$t_plus_2 = 0
        previous_not_active_activist$t_plus_3 = 0
        
        previous_not_active_activist$in_same_company = 0
        
      }
      
      # Next period: 
      
      next_not_active_activist = next_network[ which( next_network$activist  %in% not_active_activists ), ]
      
      if( nrow(next_not_active_activist) > 0 ){
        
        next_not_active_activist$campaign_id = c 
        next_not_active_activist$treated = 0
        next_not_active_activist$win = campaign_outcome
        next_not_active_activist$time_end  = ifelse( next_not_active_activist$date == ending.quarter , 1, 0 )
        next_not_active_activist$time_start  = ifelse( next_not_active_activist$date == beginning.quarter , 1, 0 )
        next_not_active_activist$t = 0
        next_not_active_activist$t_minus_1 = 0
        next_not_active_activist$t_minus_2 = 0
        next_not_active_activist$t_minus_3 = 0
        next_not_active_activist$t_plus_1 = ifelse( next_not_active_activist$date == next_periods[1] , 1, 0 )
        next_not_active_activist$t_plus_2 = ifelse( next_not_active_activist$date == next_periods[2] , 1, 0 )
        next_not_active_activist$t_plus_3 = ifelse( next_not_active_activist$date == next_periods[3] , 1, 0 )
        next_not_active_activist$in_same_company = 0
        
      }
      
      # Gather all the control sample
      
      control = rbind( previous_not_active_activist, nw_not_active_activist )
      control = rbind( control, next_not_active_activist )
      
      # bind treatment and control together: 
      out = rbind( treatment, control )
      o = rbind(o, out)

      }
      
      campaign_t_c = rbind(campaign_t_c, o)
  }

  campaign_t_c = unique(campaign_t_c)
}

saveRDS( campaign_t_c, file = "panel_win_lose_top20" )

# save(campaign_t_c, file="campaign_t_c_top20")

load("campaign_t_c_top20")
didreg_3 = lm( s ~treated+win+in_same_company+time_end,  data = campaign_t_c )
didreg_3 = felm( num_con ~
                 win*time_end*treated*in_same_company,
                 data = campaign_t_c )
summary(didreg_3)








#----------------------------------------------------
# Construct a database to test that I can do DiD
#----------------------------------------------------

