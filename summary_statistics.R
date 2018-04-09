rm(list=ls())
gc()

library(dplyr)
library(xtable)
library(dplyr)
library(plyr)
library(ggplot2)
library(lmtest)
library(reshape)
library(reshape2)
library(gridExtra)
library(Hmisc)
library(sfsmisc)
library(gdata)
library(scales)
library(tables)

'%!in%' <- function(x,y)!('%in%'(x,y))

setwd("D:/Dropbox/Activist paper/Analysis")

#-------------------------------------------------
# Create summary table for the merged subsample
#-------------------------------------------------


classified <- read.csv("D:/Dropbox/Activist paper/Analysis/classified.2015.csv")
classified$announce_date <- as.Date(classified$announce_date, "%m/%d/%Y")
classified.2015 <- classified[(classified$announce_date< "2016-1-1"),]
classified.2015 <- classified.2015[, -1]
load("D:/Dropbox/Activist paper/Analysis/shark.sub.all")

# Creat summary table after merge

load("D:/Dropbox/Activist paper/Analysis/short.data.compust_2000_2015_great_march")
classified_merged = short.data.compust

sum.table <- data.frame()

gen.under = classified_merged[which(classified_merged$activist_objective_1 == "General undervaluation/maximize shareholder value"),]
general.undervaluation <- cbind("General undervaluation/maximize shareholder value" , nrow(gen.under),   percent(nrow(gen.under)/nrow(classified_merged) ), "NA" ) 
sum.table = rbind (sum.table, general.undervaluation)
names(sum.table) = c("objective", "total.number", "percentage", "success.rate")

obj.list = unique(classified_merged$activist_objective_1) 

# include everything but General undervaluation in the list

ol = c( "Excess cash, under-leverage, dividends/repurchases", "Equity issuance, restructure debt, recapitalization",
        "Operational efficiency", "Lack of focus, business restructuring and spinning off", "M&A: as target (against the deal/for better terms)",
        "M&A: as acquirer (against the deal/for better terms)", "Pursue growth strategies", "Sell company or main assets to a third party", 
        "Take control/buyout company and/or take it private", "Rescind takeover defenses", "Oust CEO, chairman", "Board independence and fair representation",
        "More information disclosure/potential fraud", "Excess executive compensation/pay for performance","Institute enviromental protection policy")

sum.table2 = data.frame()

perc <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

for( objective in ol ){
  
  ex.cash = classified_merged[which(classified_merged$activist_objective_1 == objective | classified_merged$activist_objective_2 == objective | classified_merged$activist_objective_3 == objective),]
  
  suc = ex.cash[which(ex.cash$success_objective_1 %in% c("Success", "Partial") & ex.cash$activist_objective_1 == objective | ex.cash$success_objective_2 %in% c("Success", "Partial") & ex.cash$activist_objective_2 == objective  | ex.cash$success_objective_3 %in% c("Success", "Partial") & ex.cash$activist_objective_3 == objective ),]
  
  total.number = nrow(ex.cash)
  success.rate <- percent(nrow(suc)/nrow(ex.cash))
  percentage <- perc( nrow(ex.cash)/nrow(classified_merged) )
  # percentage = round(percemtage, 1)
  
  output <- cbind(objective , total.number, percentage , success.rate )
  
  sum.table2 <- rbind(sum.table2, output)
}

non.gen.under = classified_merged[which(classified_merged$activist_objective_1 != "General undervaluation/maximize shareholder value"),]
total.number_all <- nrow(non.gen.under)
percentage_all <- percent(total.number_all/nrow(classified_merged))
suc = non.gen.under[which(non.gen.under$success_objective_1 %in% c("Success", "Partial") | non.gen.under$success_objective_2 %in% c("Success", "Partial")  | non.gen.under$success_objective_3 %in% c("Success", "Partial")  ),]
success_all = percent( nrow(suc)/nrow(non.gen.under))
sum = as.data.frame ( cbind( "Total number", total.number_all, percentage_all, success_all) )
names(sum) = names(sum.table2)

sum_bear <- data.frame()
bear = classified.2015[which(classified.2015$activist_objective_1 ==  "Public Short Position/Bear Raid"),]
bear_short <- cbind("Public Short Position/Bear Raid" , nrow(bear), perc( nrow(bear)/nrow(classified.2015) ), "NA" ) 
sum_bear = rbind (sum_bear, bear_short)
names(sum_bear) = c("objective", "total.number", "percentage", "success.rate")


sum.table2 = rbind(sum.table2, sum_bear)
sum.table2 = rbind(sum.table2, sum)
sum.table = sum.table2

sum.table<-as.data.frame(sum.table)

alt_vars <- cbind(
  Rowgrp2 = c("Capital structure", "",
              "Business strategy", "", "", "", "", 
              "Sell company", "", 
              "Governance", "", "", "", "",
              "Other", "", ""),
  # Rownames= rownames(sum.table), 
  sum.table)

sum.table_tex_short <- xtable(alt_vars, include.rownames=TRUE, digits=1)


print(sum.table_tex_short,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames = FALSE,
      type="latex",
      
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      file="D:/Dropbox/Activist paper/Analysis/new_code/tables/sum.table_tex_short.tex")

#----------------------------------------
# Campaigns by type 
#----------------------------------------

load("shark.sub.all.2015")

# Success rates by  merged data
load("short.data.compust_2000_2015_great_march")

dem = short.data.compust[which(short.data.compust$exit_stage == "demand_negot"),]
demand_negot_count = nrow(dem)
dem_negot_percent = percent( demand_negot_count/nrow(short.data.compust))
dem_suc = dem[which(dem$success_objective_1 %in% c("Success", "Partial") | dem$success_objective_2 %in% c("Success", "Partial")  | dem$success_objective_3 %in% c("Success", "Partial") ),]
dem_success_number = nrow(dem_suc)
dem_success_percent = percent(dem_success_number/nrow(dem))
demand_negotiations = cbind("Demand negotiations", demand_negot_count, dem_negot_percent,dem_success_number, dem_success_percent)

brep = short.data.compust[which(short.data.compust$exit_stage == "board_repres"),]
board_rep_count = nrow(brep)
board_rep_percent = percent( board_rep_count/nrow(short.data.compust))
brep_suc = brep[which(brep$success_objective_1 %in% c("Success", "Partial") | brep$success_objective_2 %in% c("Success", "Partial")  | brep$success_objective_3 %in% c("Success", "Partial") ),]
brep_success_number = nrow(brep_suc)
brep_success_percent = percent(brep_success_number/nrow(brep))
board_representation = cbind("Board representation", board_rep_count, board_rep_percent,brep_success_number, brep_success_percent)

pfight = short.data.compust[which(short.data.compust$exit_stage == "proxy_fight"),]
pfight_count = nrow(pfight)
pfight_percent = percent( pfight_count/nrow(short.data.compust))
pfight_suc = pfight[which(pfight$success_objective_1 %in% c("Success", "Partial") | pfight$success_objective_2 %in% c("Success", "Partial")  | pfight$success_objective_3 %in% c("Success", "Partial") ),]
pfight_success_number = nrow(pfight_suc)
pfight_success_percent = percent(pfight_success_number/nrow(pfight))
proxy_fight = cbind("Proxy fight", pfight_count, pfight_percent,pfight_success_number, pfight_success_percent)

thpfight = short.data.compust[which(short.data.compust$exit_stage == "treaten_fight"),]
thpfight_count = nrow(thpfight)
thpfight_percent = percent( thpfight_count/nrow(short.data.compust))
thpfight_suc = thpfight[which(thpfight$success_objective_1 %in% c("Success", "Partial") | thpfight$success_objective_2 %in% c("Success", "Partial")  | thpfight$success_objective_3 %in% c("Success", "Partial") ),]
thpfight_success_number = nrow(thpfight_suc)
thpfight_success_percent = percent(thpfight_success_number/nrow(thpfight))
treaten_proxy_fight = cbind("Threaten proxy fight", thpfight_count, thpfight_percent,thpfight_success_number, thpfight_success_percent)

buyc = short.data.compust[which(short.data.compust$exit_stage == "buy_company"),]
buyc_count = nrow(buyc)
buyc_percent = percent( buyc_count/nrow(short.data.compust))
buyc_suc = buyc[which(buyc$success_objective_1 %in% c("Success", "Partial") | buyc$success_objective_2 %in% c("Success", "Partial")  | buyc$success_objective_3 %in% c("Success", "Partial") ),]
buyc_success_number = nrow(buyc_suc)
buyc_success_percent = percent(buyc_success_number/nrow(buyc))
buy_company = cbind("Buy company", buyc_count, buyc_percent,buyc_success_number, buyc_success_percent)

# success_stage_merged = rbind(demand_negotiations,board_representation,treaten_proxy_fight, proxy_fight, buy_company)
success_stage_merged = rbind(demand_negotiations,board_representation, proxy_fight)
success_stage_merged = as.data.frame(success_stage_merged )
success_stage_merged_tex <- xtable(success_stage_merged ,include.rownames=TRUE, digits=2)

print(success_stage_merged_tex,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames = FALSE,
      type="latex",
      
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      file="D:/Dropbox/Activist paper/Analysis/new_code/tables/success_stage_merged_tex.tex")



# ----------------------------------------
#Create summary table for final subsample
# ----------------------------------------

load("short.data.compust_2000_2015")

short.summary <- data.frame()

checked_board_seats_won = short.data.compust$checked_board_seats_won
won_board_ind = short.data.compust$won_board_ind
success_of_stated_obj = short.data.compust$success_of_stated_obj
sales_growth = short.data.compust$sales_growth
oper_profit_growth = short.data.compust$oper_profit_growth
active.activist.number = short.data.compust$active.activist.number
surv_act_appearance_number = as.numeric( short.data.compust$surv_act_appearance_number )

campaign_outcomes <- list( success_of_stated_obj, won_board_ind)  
names_campaign_outcomes <- c("success_of_stated_obj", "won_board_ind")  

for (i in 1:length(campaign_outcomes) ){
  variable = (campaign_outcomes)[[i]]
  name = names_campaign_outcomes[i]
  out <- cbind( mean(variable, na.rm = TRUE),sd(variable, na.rm = TRUE), min(variable, na.rm = TRUE), quantile(variable, 0.25, na.rm = TRUE), 
                median(variable, na.rm = TRUE), quantile(variable, 0.75, na.rm = TRUE),max(variable, na.rm = TRUE))
  out <- round(out, 2)
  out = cbind(name,out)
  short.summary = rbind(short.summary, out)}

active.activist.size=short.data.compust$active.activist.size

netw_log <- short.data.compust[c("dissident_group_ownership_percent","log_active.activist_norm_weight","active.activist.number", "surv_act_appearance_number", "top5_number", "log_act_num_con", 
                                 "top5_perc",
                                 # "log_top5_share",
                                 "log_top5_size_nw_s", "log_top5_share_nw_spr",
                                 "log_top5_w_norm_s", 
                                 "log_top5_w_norm_spr", 
                                 "log_top5_s_clos_inv", "log_top10_s_clos_inv", "log_top5_s_betw_inv", "log_top5_sp_betw_inv")]

names_netw_log <- c("dissident_group_ownership_percent","log_active.activist_norm_weight", "active.activist.number","surv_act_appearance_number", "top5_number", 
                    "log_act_num_con",
                    # "top5_perc",
                    "log_top5_share",
                    "log_top5_share_nw_s", "log_top5_share_nw_spr",
                    "log_top5_w_norm_s", "log_top5_w_norm_spr", 
                    "log_top5_s_clos_inv", "log_top10_s_clos_inv", "log_top5_s_betw_inv", "log_top5_sp_betw_inv")

for (i in 1:length(netw_log) ){
  variable = (netw_log)[[i]]
  variable = variable[!is.na(variable)]
  name = names_netw_log[i]
  # variable=log(variable)
  # print(i)
  if(sum(variable==-Inf) >0){
    variable[variable==-Inf] = 0
  }
  out <- cbind( mean(variable, na.rm = TRUE),sd(variable, na.rm = TRUE), min(variable, na.rm = TRUE), quantile(variable, 0.25, na.rm = TRUE), 
                median(variable, na.rm = TRUE), quantile(variable, 0.75, na.rm = TRUE),max(variable, na.rm = TRUE))
  out = round(out, 2)
  out = cbind( name, out)
  short.summary = rbind(short.summary, out)
}


load("investor_networks_summary_all_top5")
names(investor_networks_summary_all) = names(short.summary) 
short.summary = rbind(short.summary, investor_networks_summary_all)


controls = short.data.compust[c("size")]

name_controls = c("log(size)")

for (i in 1:length(controls) ){
  variable = (controls)[[i]]
  name = name_controls[i]
  out <- cbind( mean(log(variable), na.rm = TRUE), sd(log(variable), na.rm = TRUE),
                min(log(variable), na.rm = TRUE), 
                quantile(log(variable), 0.25, na.rm = TRUE),
                median(log(variable), na.rm = TRUE), 
                quantile(log(variable), 0.75, na.rm = TRUE),
                max(log(variable), na.rm = TRUE))
  out =round(out, 2)
  out = cbind(  name, out)
  short.summary = rbind(short.summary, out)}


controls = short.data.compust[c("poison_pill", "age","leverage","mtb","real_roa", "short_term", "exit_s_board", "exit_s_proxy"  )]

name_controls = names(controls)

for (i in 1:length(controls) ){
  variable = (controls)[[i]]
  name = name_controls[i]
  out <- cbind( mean(variable, na.rm = TRUE),sd(variable, na.rm = TRUE), min(variable, na.rm = TRUE), quantile(variable, 0.25, na.rm = TRUE), 
                median(variable, na.rm = TRUE), quantile(variable, 0.75, na.rm = TRUE),max(variable, na.rm = TRUE))
  out =round(out, 2)
  out = cbind( name, out)
  short.summary = rbind(short.summary, out)}

names(short.summary) = c("Variable", "Mean", "Median", "Sd")
row.names(short.summary) <- NULL
short.summary <- as.data.frame( short.summary)
names(short.summary) <- c( "Variable", "min","25%","Mean", "Median", "75%", "max", "Sd")

all_variables = short.summary$Variable

vector = c ("Success of stated goals", "Won board seat(s)", "Perc. active ownership","Import. activist ownership", "Num.activists", "Num. campaigns by activist","Num. large investors",
            "log(Num. activist connections)", "Perc. large ownership", "Simple con. weighted by perc.", "RI con. weighted by perc.",
            "Simple con. weighted by import.", "RI con. weighted by import.", 
            "Simple closeness centrality", "RI closeness centrality", "Simple betweenness centrality", "RI betweenness centrality", 
            "RI strength", "Simple strength",
            "log(Market capitalization)", "Poison pill", "Firm age", "Leverage", "MTB", "ROA", "Short term objective", "Exit after board demands", 
            "Exit after proxy fight")
short.summary$Variable = vector


short.summary<-as.data.frame(short.summary)
short.summary$Mean[6] = 1 

alt <- cbind(
  Rowgrp2 = c("Campaign outcome", "",
              "Activist power", "", "","",
              "Network", "", "", "", "",  "", "", "", "",  "", "", "", "",
              "Controls", "", "", "", "",  "", "", 
              "Campaign cost", ""),
  # Rownames= rownames(sum.table), 
  short.summary)


short_summary_tex <- xtable(alt,include.rownames=TRUE, digits=2)

print(short_summary_tex,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames = FALSE,
      type="latex",
      
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      file="D:/Dropbox/Activist paper/Analysis/new_code/tables/short_summary_tex.tex")



# ----------------------------------------
# Do a correlation table
# ----------------------------------------

cortable = short.data.compust[c ("success_of_stated_obj",  "won_board_ind", "active.activist.share",
                                "log_active.activist_norm_weight", "top5_number", "act_num_con", "top5_percent", 
                                "log_top5_share_nw_s", "log_top5_share_nw_spr", "log_top5_w_norm_s", "log_top5_w_norm_spr",   
                                "log_top5_s_clos_inv", "log_top10_s_clos_inv", "log_top5_s_betw_inv", "log_top5_sp_betw_inv",
                                "log_size", "poison_pill", "age", "leverage", "mtb","roa", "short_term")]


vec = c ("Success of stated goals", "Won board seat(s)", "Perc. active ownership","Import. activist ownership", "Num. large investors",
         "log(Num. activist connections)", "Perc. large ownership", "Simple con. weight by perc.", "RI con. weight by perc.",
         "Simple con. weight by import.", "Simple con. weight by import.", 
         "Simple closeness centrality", "RI closeness centrality", "Simple betweenness centrality", "RI betweenness centrality",
         "log(Market capitalization)", "Poison pill", "Firm age", "Leverage", "MTB",
         "ROA", "Short term objective")

upper=round( cor(cortable, use="pairwise.complete.obs"), 2)
upper[upper.tri(upper)]<-""

upper<-as.data.frame(upper)
names(upper) <- c(1:nrow(upper))
rownames(upper) = paste0(1:nrow(upper)," ", vec)
upper_tex <- xtable(upper,include.rownames=TRUE, digits=2)

print(upper_tex,
      only.contents=TRUE,
      include.rownames=TRUE,
      include.colnames = TRUE,
      type="latex",
      size="\\fontsize{8pt}{8pt}\\selectfont",
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      file="D:/Dropbox/Activist paper/Analysis/new_code/tables/upper_tex.tex")
