rm(list=ls())
gc()
setwd("C:/Users/anakhmur/Dropbox/Analysis")

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



#------------------------Do a DID with a control group of activist herself when not active

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
 
  
  for(activist in activist_cik){
    # print(activist)
    
    # load start network files 
    announce = unique(campaign_data$announce_date)
    start_year <- unique( substr(announce, 1, 4))
    
    out <- data.frame()
    if(start_year > 1999){
      
      beginning.quarter <-unique( quarters.list[ findInterval(announce, quarters.list) + 1 ] )
      data_13f = cik_13f[ which(cik_13f$date == beginning.quarter & cik_13f$cusip6 == firm_cusip), ]
      connect_list <- setdiff( unique(data_13f$cik), activist )
      
      start_period <- unique( substr(beginning.quarter, 6, 10) )
      start_quarter <- corr$quarter[which(corr$period == start_period)]
      
      file = paste0("investor_network_top20_", start_quarter, "_", start_year)
      load(paste0("C:/Users/anakhmur/Dropbox/Analysis/networks/", file))
      
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
        
        
        file = paste0("investor_network_top20_", next_quarter, "_", end_year)
        load(paste0("C:/Users/anakhmur/Dropbox/Analysis/networks/", file))
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
        }else{ if(nrow(fn_init) == 0 & nrow(fn_next) >0){
            
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
        
        # o  = NAer(o)
        
        
        out <- rbind(out, o)
      }
      
    }
    
    # outcomes <- rbind(out, outcomes)
    
    # select the time when activist didn't do campaigns
    
    war_periods = clean.shark.final[which(clean.shark.final$cik == activist ), ]
    
    war_periods = war_periods[c("announce_date", "end_date")]
    
    war_dates <- vector()
    
    for(j in 1: nrow(war_periods) ){
      w = war_periods[j,]
      
      quarters = ( quarters.list[ findInterval(w, quarters.list) + 1 ] )
      quarter_period = quarters.list[which(quarters.list>= quarters[1] 
                                           & quarters.list <= quarters[2] )]
      quarter_period = as.character(quarter_period)
      war_dates = c(war_dates, quarter_period)
    }
    
    war_dates = unique(war_dates)
    war_dates = as.Date(war_dates, "%Y-%m-%d")
    
    peace_dates = unique(quarters.list[which(quarters.list %!in% war_dates)] )
    ##### STOPPED HERE
    
    # Return the length of the campaign
    campaign_length = length( quarters.list[which(quarters.list>  beginning.quarter & quarters.list < ending.quarter)] )
    
    # For activist control, select periods where activist was peaceful for the length of campaign
    
    activist_control = data.frame()
    
    for( d in 1:length(peace_dates) ){
      print("peace")
      # print(d)
      # d=5
      s = peace_dates[d]
      e =d+campaign_length-1
      peace_period = peace_dates[d:e]
      check = quarters.list[ which( quarters.list >= s) ]
      check = check[1:campaign_length]
      
      condition = sum(peace_period == check)
      
      if( condition == campaign_length & !is.na(condition) ){
        
        # print("passed")
          
          date = peace_period[1]
          start_q = substr(date, 6,10)
          start_quarter = corr$quarter[which(corr$period == start_q)]
          start_year = substr(date, 1,4)
          
          file = paste0("investor_network_top20_", start_quarter, "_", start_year)
          load(paste0("C:/Users/anakhmur/Dropbox/Analysis/networks/", file))
          
          fn_init <- investor_network[which(investor_network$activist %in% activist ),]
          
          if( nrow(fn_init)>0 ){
            fn_init$date = date
            names(fn_init) = c("activist_cik", "connect_cik", paste0("init_", names(fn_init)[3:5]))
            fn_init$campaign_outcome = 0
            fn_init$in_same_company = 0
          }
          
          # activist_control = rbind(activist_control,fn_init)
          
          end_date =  peace_period[length(peace_period)]
          end_q = substr(end_date, 6,10)
          end_quarter = corr$quarter[which(corr$period == end_q)]
          end_year = substr(end_date, 1,4)
          
          file = paste0("investor_network_top20_", end_quarter, "_", end_year)
          load(paste0("C:/Users/anakhmur/Dropbox/Analysis/networks/", file))
          
          fn_next <- investor_network[which(investor_network$activist %in% activist ),]
          
          if( nrow(fn_next)>0 ){
            fn_next$date = end_date
            names(fn_next) = c("activist_cik","connect_cik", paste0("next_", names(fn_next)[3:5]) )
            fn_next$in_same_company = 0
          }
          
          if( nrow(fn_init) >0 & nrow(fn_next) >0 ){
            o  = merge( fn_init, fn_next, all = T )
            o$campaign_id = campaign
            o$treated <- 0
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
              o$treated <- 0
            }else{if( nrow(fn_next)==0 & nrow(fn_init) >0 ){
              df = data.frame(matrix(NA, nrow=nrow(fn_init), ncol=5))
              names(df) <- database_names[7:11]
              df$activist_cik = fn_init$activist_cik
              df$connect_cik = fn_init$connect_cik
              df$next_date = next_after_end
              o  = merge( fn_init, df, all =T )
              o$campaign_id = campaign
              o$treated <- 0
            }else{ o <-data.frame() }
            }
          }
     
          activist_control <- rbind(activist_control, o)
          
    }
    
      
  }
  
    
  
  outcomes = rbind(out, activist_control)
  
  outcomes_t_c = rbind(outcomes_t_c, outcomes)
  
}

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

save(outcomes_t_c, file="outcomes_t_s_top20_activist_control")

# t = complete.cases(outcomes_t_c)


library(data.table)
library(psych)
library(dummies)
library(lfe)


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

hist(g0$diff_number,  breaks = 500,  col="gray")
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
save(winlose, file= "winlose_self_control")


did_3 =  lm( diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company , 
             data = winlose )
did_3 =  lm( diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company+factor(campaign_id) , 
             data = winlose )
did_3 =  felm( diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company | campaign_id+year , 
               data = winlose )
summary(did_3)

