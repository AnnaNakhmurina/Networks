rm(list=ls())
gc()

library(stargazer)
library(sandwich)
library(multiwayvcov)
library(lfe)
library(plm)
library(data.table)


setwd("D:/Dropbox/Activist paper/Analysis")

load("D:/Dropbox/Activist paper/Analysis/short.data.compust_2000_2015")

# Get rid of an accidental duplicate

short.data.compust = as.data.table(short.data.compust)
test = short.data.compust[ , c("campaign.id", "success_of_stated_obj" , "log_top5_number", "log_top10_share_nw_s",
                               "log_top5_percent", "log_top5_share_nw_s", "log_top5_share_nw_spr", "log_top5_w_norm_s",
                               "log_top5_w_norm_spr", "log_top5_s_clos_inv", "top5_share_nw_s",
                               "log_top5_sp_clos_inv", "log_top5_s_betw_inv", "log_top5_sp_betw_inv",
                               "log_active.activist_norm_weight", "log_top5_perc",
                               "log_act_num_con", "active.activist.share", "exit_s_board",
                               "exit_s_proxy", "poison_pill",  "age", "size", "leverage", "mtb", "roa",
                               "short_term", "surv_act_appearance_number",  "year", "holder_type", "type1",
                               "log_top10_sp_clos_inv", "log_top20_sp_clos_inv"
                               ) ]

complete = test[ complete.cases(test) ]
complete = unique(complete)

data = complete

#--------------------------------------
# Just number and share
#--------------------------------------

reg1 <- felm( success_of_stated_obj ~ log_top5_number + log(active.activist.share) + exit_s_board + 
                                      exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                                      roa + short_term + surv_act_appearance_number + year | holder_type | 0 | type1  , 
                                      data = data )

summary(reg1)

reg2 <- felm( success_of_stated_obj ~ log_act_num_con + log(active.activist.share) + exit_s_board + 
                                      exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                                      roa + short_term + surv_act_appearance_number + year | holder_type | 0 | type1  , 
                                      data = data )

summary(reg2)

reg3 <- felm( success_of_stated_obj ~ log_top5_number + log_act_num_con +  log(active.activist.share)+ exit_s_board + 
                exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                roa + short_term + surv_act_appearance_number + year | holder_type | 0 | type1  , 
              data = data )

summary(reg3)

reg4 <- felm( success_of_stated_obj ~ log_top5_perc + log(active.activist.share) + exit_s_board + 
                exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                roa + short_term + surv_act_appearance_number + year | holder_type | 0 | type1  , 
              data = data )
summary(reg4)

reg5 <- felm( success_of_stated_obj ~ log_top5_perc + log_act_num_con +log(active.activist.share) + exit_s_board + 
                exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                roa + short_term + surv_act_appearance_number + year | holder_type | 0 | type1  , 
              data = data )
summary(reg5)


ols3_output = capture.output(stargazer( 
                                       reg1, reg2, reg3, reg4, reg5,
                                       type = "latex",
                                       title = "Number of large investors and activist's success",
                                       font.size = "tiny", float.env="table", header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       report = "vct*",
                                       digits = 4,
                                       dep.var.labels   = c("Success of stated goals"),
                                       add.lines = list( c("Activist type FE ", "Yes", "Yes", "Yes", "Yes", "Yes") )
                                       , covariate.labels = c("Num.large investors", "Perc. large ownership",
                                                            "Num. activist connections",
                                                            ,"Exit after board demands",
                                                            "Exit after proxy fight", "Perc. activist ownership",
                                                            "Poison pill", "Firm age",
                                                            "log(Market capitalization)", "Leverage",
                                                            "MTB", "ROA", "Short term objective",
                                                            "Num. campaigns by activist"
                                                            #, "Exit after board demands x Perc. activist ownership",
                                                            # " Exit after proxy fight x Perc. activist ownership"
                                       )
                                       , table.placement = "H" ))                                               

note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{Success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands.  \\textit{Num. large investors} is the number of investors that fall into top 5\\% holding percentile and have shares in the target. \\textit{Num. activist connections} is total number of connections that activist has with large investors in the target.  \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01 }} \\\\"
ols3_output[grepl("Note",ols3_output)] <- note.latex

writeLines( ols3_output, "D:/Dropbox/Activist paper/Analysis/new_code/tables/table_5.tex" )

#--------------------------------------
# Share weighted
#--------------------------------------

support1 = felm( success_of_stated_obj ~ log_top5_share_nw_s + exit_s_board + 
                                         exit_s_proxy + log(active.activist.share)  + poison_pill + age +  log(size) + leverage + mtb + 
                                        log(active.activist.share)*exit_s_board+log(active.activist.share)*exit_s_proxy +
                                         roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
                                         data = data )

summary(support1)

support2 = felm( success_of_stated_obj ~ log_top5_share_nw_spr + exit_s_board + 
                  exit_s_proxy +  log(active.activist.share) + poison_pill + age +  log(size) + leverage + mtb + 
                   log(active.activist.share)*exit_s_board+log(active.activist.share)*exit_s_proxy +
                  roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
                  data = data )

summary(support2)

support3 = felm( success_of_stated_obj ~ log_top5_w_norm_s + exit_s_board + 
                   exit_s_proxy  + log_active.activist_norm_weight + poison_pill + age +  log(size) + leverage + mtb + 
                   log_active.activist_norm_weight*exit_s_board+log_active.activist_norm_weight*exit_s_proxy +
                   roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
                 data = data )

summary(support3)

support4 = felm( success_of_stated_obj ~ log_top5_w_norm_spr  + exit_s_board + 
                   exit_s_proxy + log_active.activist_norm_weight + poison_pill + age +  log(size) + leverage + mtb + 
                   log_active.activist_norm_weight*exit_s_board+log_active.activist_norm_weight*exit_s_proxy +
                   roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
                   data = data )

summary(support4)


ols3_output = capture.output(stargazer(support1, support2, support3, support4,
                                       type = "latex",
                                       title = "Network support and activist's success",
                                       font.size = "tiny", float.env="table",header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       report = "vct*",
                                       digits = 4,
                                       dep.var.labels   = c("Success of stated goals"),
                                       covariate.labels = c("Simple con. weighted by perc.", "RI con. weighted by perc.",
                                                            "Simple con. weighted by import.", "RI con. weighted by import.",
                                                            "Exit after board demands",
                                                            "Exit after proxy fight", "Perc. activist ownership",
                                                            "Import. activist ownership",
                                                            "Poison pill", "Firm age",
                                                            "log(Market capitalization)", "Leverage", "MTB", "ROA", "Short term objective",
                                                            "Perc. large ownership", "Num. campaigns by activist"
                                                            # ,"Exit after board demands x Perc. activist ownership",
                                                            # " Exit after proxy fight x Perc. activist ownership",
                                                            #   "Exit after board demands x Import. activist ownership",
                                                            # " Exit after proxy fight x Import. activist ownership"
                                       ),
                                       add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes", "Yes"))
                                       ,omit = c("holder_type", "year", ":")
                                       ,table.placement = "H"))

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands.  \\textit{Simple con. weighted by perc.} corresponds to the simple strength of connection weighted by large investor's ownership in the target and aggregated across large investors.
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\textit{Simple con. weighted by import.}  corresponds to the simple strength of connection weighted the importance of target to large investors and aggregated across large investors. \\textit{RI con. weighted by import.} corresponds to the relative influence strength of connection weighted the importance of target to large investors and aggregated across large investors. Importance is defined as $ \\frac{Share - min\\{Share\\}}{\\max\\{Share\\} - \\min\\{Share\\}}$.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01  }} \\\\"
ols3_output[grepl("Note",ols3_output)] <- note.latex

writeLines( ols3_output, "D:/Dropbox/Activist paper/Analysis/new_code/tables/table_6.tex" )


#--------------------------------------
#  centrality measures (2 closeness measures) 
#--------------------------------------

# Just in case I would want to check with the original march version
# load("D:/Dropbox/Activist paper/Analysis/short.data.compust_2000_2015_original march_version")
# 
# old = short.data.compust
# data = old

centr1 = felm( success_of_stated_obj ~ log_top5_s_clos_inv + top5_share_nw_s + log_active.activist_norm_weight + 
                   exit_s_board + 
                   exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                   # log_active.activist_norm_weight*exit_s_board+log_active.activist_norm_weight*exit_s_proxy +
                   roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
                   data = data )

summary(centr1)

centr2 = felm( success_of_stated_obj ~ log_top5_sp_clos_inv + top5_share_nw_s + log_active.activist_norm_weight + 
                 exit_s_board + 
                 exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                 # log_active.activist_norm_weight*exit_s_board+log_active.activist_norm_weight*exit_s_proxy +
                 roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
               data = data )

summary(centr2)

centr3 = felm( success_of_stated_obj ~ log_top5_s_betw_inv + top5_share_nw_s + log_active.activist_norm_weight + 
                 exit_s_board + 
                 exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                 # log_active.activist_norm_weight*exit_s_board+log_active.activist_norm_weight*exit_s_proxy +
                 roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
                 data = data )

summary(centr3)


centr4 = felm( success_of_stated_obj ~ log_top5_sp_betw_inv + top5_share_nw_s + log_active.activist_norm_weight + 
                 exit_s_board + 
                 exit_s_proxy + poison_pill + age +  log(size) + leverage + mtb + 
                 # log_active.activist_norm_weight*exit_s_board+log_active.activist_norm_weight*exit_s_proxy +
                 roa + short_term + log_top5_percent + surv_act_appearance_number + year | holder_type | 0 | type1, 
               data = data )

summary(centr4)


centr = capture.output(stargazer(centr1, #centr2,  
                                 centr3, centr4,
                                 type = "latex",
                                 dep.var.labels   = c("Success of stated goals"),
                                 title = "Activist investors' centrality and activist's success",
                                 covariate.labels = c("Simple closeness centrality", #"RI closeness centrality",
                                                      "Simple betweenness centrality", "RI betweenness centrality",
                                                      "RI con. weighted by perc.",
                                                      "Import. activist ownership",
                                                      "Exit after board demands",
                                                      "Exit after proxy fight",
                                                      "Poison pill", "Firm age",
                                                      "log(Market capitalization)", "Leverage", "MTB", "ROA", "Short term objective", "Num. campaigns by activist"),
                                 add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes", "Yes")), 
                                 font.size = "tiny", float.env="table",header = FALSE,
                                 omit.stat=c("f", "ser"),
                                 omit = c("year"), 
                                 report = "vct*",
                                 digits = 4,
                                 table.placement = "H"
                                 ))

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{15cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha + \\beta_1Investor\\_importance+ \\beta_2Network\\_support + \\beta_3Activist\\_power + \\beta_4Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands. \\textit{Simple closeness centrality} is an aggregate closeness centrality computed with simple network. \\textit{RI closeness centrality} is an aggregate closeness centrality computed with relative influence network. \\textit{Simple betweennes centrality} is an aggregate betweennes centrality computed with simple network. \\textit{RI betweenness centrality} is an aggregate betweennes centrality computed with relative influence network. 
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01 }} \\\\"
centr[grepl("Note",centr)] <- note.latex

writeLines( centr, "D:/Dropbox/Activist paper/Analysis/new_code/tables/table_7.tex" )


#--------------------------------------
# Diff-in-diff in number of connections
#--------------------------------------

load("winlose_top20_control_other")

winning = winlose[ which(winlose$win == 1 ), ]
losing = winlose[ which(winlose$win == 0 ), ]

did_20_win = felm( diff_number ~ treated + in_same_company+ treated*in_same_company    |  activist_cik+year| 0 | campaign_id , data = winning )

summary( did_20_win )

did_20_lose = felm( diff_number ~ treated + in_same_company+ treated*in_same_company    |  activist_cik+year| 0 | campaign_id , data = losing )

summary( did_20_lose )


did_30 = felm( diff_number ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company 
              |  activist_cik+year| 0 | campaign_id , data = winlose )

summary( did_30 )

# se_30  = sqrt(diag(  cluster.vcov(did_30, winlose$campaign_id) ) )

did_21_win = felm( diff_s ~ treated + in_same_company+ treated*in_same_company    |  activist_cik+year| 0 | campaign_id , data = winning )

summary( did_21_win )

did_21_lose = felm( diff_s ~ treated + in_same_company+ treated*in_same_company    |  activist_cik+year| 0 | campaign_id , data = losing )

summary( did_21_lose )

did_31 = felm(diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company 
              |  activist_cik+year| 0 | campaign_id , data = winlose)

summary( did_31 )

did_out = capture.output(stargazer(  did_20_win, did_20_lose, did_30,
                                     did_21_win, did_21_lose, did_31,
                                     type = "latex",  
                                     title = "Activist's success and connection strength. Difference-in-difference analysis.",
                                     font.size = "tiny",
                                     float.env="table",header = FALSE,
                                     omit.stat=c("f", "ser"),
                                     report = "vc*t",
                                     digits = 2,
                                     omit = c("activist_cik", "year"),
                                     add.lines = list(c("Activist FE ", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                      c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                     covariate.labels = c("Connected to active activist", "Activist wins",
                                                          "Target shareholder", "Connected to active activist x Target shareholder",
                                                          "Connected to active activist x Activist wins",
                                                          "Connected to active activist x Target shareholder",
                                                          "Connected to active activist x Target shareholder x Activist wins"),
                                     dep.var.labels   = c("$\\Delta(Simple)$", "$\\Delta(RI)$"),
                                     column.labels = c("Win", "Lose", "Win+Lose","Win", "Lose", "Win+Lose"),
                                     # omit.yes.no = T,
                                     # omit.labels = "Firm specific controls?",
                                     single.row = TRUE
                                     # ,se=list(se_30, se_31 )
                                     ,table.placement = "!htbp"
))


note.latex <- "\\multicolumn{7}{l} {\\parbox[t]{18cm}{ \\textit{Notes:} OLS regression  of the equation \\\\	$	\\Delta(Connection\\_strength) = \\alpha +\\beta_1 Connected\\_to\\_active\\_activist  +\\beta_2 Activist\\_wins + \\beta_3Target\\_shareholder + \\gamma_1Connected\\_to\\_active\\_activist \\cdot Activist\\_wins+ \\gamma_2 Connected\\_to\\_active\\_activist \\cdot Target\\_shareholder+ \\gamma_3 Activist\\_wins \\cdot Target\\_shareholder+ \\delta Connected\\_to\\_active\\_activist \\cdot Activist\\_wins \\cdot Target\\_shareholder+Controls +\\epsilon $. In the first column, 	$\\Delta(Simple)$ corresponds to the change in the simple strength (number of connections). In the second column, $\\Delta(RI)$ corresponds to the change in the relative influence strength. $Connected\\_to\\_active\\_activist$ is an indicator that passive investor is connected to an active activist, $Activist\\_wins$ is a dummy of activist's victory, and $Target\\_shareholder$ is an indicator that passive investor is a shareholder of the target company. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01 }} \\\\"
did_out[grepl("Note",did_out)] <- note.latex

writeLines( did_out, "D:/Dropbox/Activist paper/Analysis/new_code/tables/did_alll.tex" )





