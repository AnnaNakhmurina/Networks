

load("all_campaigns")

load("13f_2000_2015_w_both_centr")
'%!in%' <- function(x,y)!('%in%'(x,y))

load("clean.shark.final_age")
full_activist_list <- unique(clean.shark.final$cik)
full_activist_list <- full_activist_list [which(!is.na(full_activist_list))]

# Create summary statistics regarding the fund networks
nw_files = list.files( pattern = "investor_network_top20")
nw_files = nw_files[1:64]

quarter <- c("1q", "2q", "3q", "4q")
period <- c("03-31", "06-30", "09-30", "12-31")
corr <- data.frame(quarter, period)
quarters.list = sort(unique(cik_13f$date))

fund_n <- data.frame()

for (file in nw_files){
  
  print(file)
  load( paste0(file))
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
  

}

campaign_t_c = unique(campaign_t_c)
saveRDS( campaign_t_c, file = "panel_win_lose_top20.rds" )