
# This file performs robustness regressions only. Main tests are done in the file "regressions.R".  

rm(list=ls())
gc()

library(stargazer)
library(sandwich)
library(multiwayvcov)
library(lfe)
library(plm)
library(lmtest)
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
                               "log_top10_share_nw_s", "log_top10_percent", "log_top10_share_nw_spr", "log_top10_w_norm_s",
                               "log_top10_w_norm_spr", "log_top20_share_nw_s","log_top20_share_nw_spr", "log_top20_w_norm_s",
                               "activist_obj_type_1"
                               # , "log_top10_w_sd_s"
                               # ,"log_top20_w_norm_spr", "log_top5_perc_nw_s", "log_top20_w_sd_s", "log_top5_w_sd_s" 
                               # ,"log_active.activist_sd_weight"
                               # "log_top20_w_sd_spr", "log_top10_w_sd_spr" ,"log_top5_w_sd_spr", "won_board_ind"
) ]

complete = test[ complete.cases(test) ]
complete = unique(complete)

data = complete

#-------------------------------------------------------------------------
# Share weighted. Robustness with including top10 instead of top5
#-------------------------------------------------------------------------

reg1 <- felm( success_of_stated_obj ~ log_top10_share_nw_s + exit_s_board + exit_s_proxy +
               log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
               mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
                data = data )

summary(reg1)


reg2 <- felm( success_of_stated_obj ~ log_top10_share_nw_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data )

summary(reg2)

reg3 <- felm( success_of_stated_obj ~ log_top10_w_norm_s + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data )

summary(reg3)


reg4 <- felm( success_of_stated_obj ~ log_top10_w_norm_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
                data = data )

summary(reg4)


t5_robust_top10 = capture.output(stargazer(reg1, reg2, reg3, reg4,
                                       type = "latex",
                                       title = "Robustness: network support and activist's success. Large investor is an investor that belongs to top 10 percent",
                                       font.size = "tiny", float.env="table",header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       dep.var.labels   = c("Success of stated goals"),
                                       report = "vct*",
                                       digits = 4,
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
                                       add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes", "Yes"),
                                                        c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                       omit = c("holder_type", "year"),
                                       # omit.labels = "Firm specific controls?",
                                       table.placement = "H"))

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands.  \\textit{Simple con. weighted by perc.} corresponds to the simple strength of connection weighted by large investor's ownership in the target and aggregated across large investors.
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\textit{Simple con. weighted by import.}  corresponds to the simple strength of connection weighted the importance of target to large investors and aggregated across large investors. \\textit{RI con. weighted by import.} corresponds to the relative influence strength of connection weighted the importance of target to large investors and aggregated across large investors. Importance is defined as $ \\frac{Share - min\\{Share\\}}{\\max\\{Share\\} - \\min\\{Share\\}}$.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01  }} \\\\"
t5_robust_top10[ grepl( "Note", t5_robust_top10 ) ] <- note.latex

writeLines( t5_robust_top10, "D:/Dropbox/Activist paper/Analysis/new_code/tables/robust_table_5_top10.tex" )

#-------------------------------------------------------------------------
# Share weighted. Robustness with including top20 instead of top5
#-------------------------------------------------------------------------

reg1 <- felm( success_of_stated_obj ~ log_top20_share_nw_s + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data )

summary(reg1)

reg2 <- felm( success_of_stated_obj ~ log_top20_share_nw_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data )

summary(reg2)

reg3 <- felm( success_of_stated_obj ~ log_top20_w_norm_s + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data )

summary(reg3)

reg4 <- felm( success_of_stated_obj ~ log_top20_w_norm_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data )

summary(reg4)


t5_robust_top20 = capture.output(stargazer(reg1, reg2, reg3, reg4,
                                       type = "latex",
                                       title = "Robustness: network support and activist's success. Large investor is an investor that belongs to top 20 percent",
                                       dep.var.labels   = c("Success of stated goals"),
                                       font.size = "tiny", float.env="table",header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       report = "vct*",
                                       digits = 4,
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
                                       add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes", "Yes"),
                                                        c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                       omit = c("holder_type", "year"),
                                       # omit.labels = "Firm specific controls?",
                                       table.placement = "H" ))

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{12.5cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands.  \\textit{Simple con. weighted by perc.} corresponds to the simple strength of connection weighted by large investor's ownership in the target and aggregated across large investors.
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\textit{Simple con. weighted by import.}  corresponds to the simple strength of connection weighted the importance of target to large investors and aggregated across large investors. \\textit{RI con. weighted by import.} corresponds to the relative influence strength of connection weighted the importance of target to large investors and aggregated across large investors. Importance is defined as $ \\frac{Share - min\\{Share\\}}{\\max\\{Share\\} - \\min\\{Share\\}}$.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01  }} \\\\"
t5_robust_top20[grepl("Note",t5_robust_top20)] <- note.latex

writeLines( t5_robust_top20, "D:/Dropbox/Activist paper/Analysis/new_code/tables/robust_table_5_top20.tex" )

#-------------------------------------------------------------------------
# Logit instead of OLS
#-------------------------------------------------------------------------

reg1 <- glm( success_of_stated_obj ~ log_top5_share_nw_s + exit_s_board + exit_s_proxy +
                                    log(active.activist.share) + poison_pill + age + 
                                    log(size) + leverage + mtb + roa + short_term +
                                    log_top5_percent + surv_act_appearance_number
                                    + factor(holder_type) + year,
                                    data = data, 
                                    family = "binomial"
             )
# se1   <- sqrt(diag( vcovHC(reg1) ))
se1  = sqrt(diag(  cluster.vcov( reg1, data$type1 ) ) )
coeftest( reg1, vcov. = cluster.vcov( reg1, data$type1 ) )

reg2 <- glm( success_of_stated_obj ~ log_top5_share_nw_spr + exit_s_board + exit_s_proxy +
                                     log(active.activist.share) + poison_pill + age + 
                                     log(size) + leverage + mtb + roa + short_term +
                                     log_top5_percent + surv_act_appearance_number +
                                     factor(holder_type) + year,
                                     data = data, 
                                     family = "binomial"
             )
se2  = sqrt(diag(  cluster.vcov(reg2, data$type1) ) )
coeftest( reg2, vcov. = cluster.vcov( reg2, data$type1 ) )

reg3 <- glm(success_of_stated_obj ~ log_top5_w_norm_s + exit_s_board + exit_s_proxy +
                                    log(active.activist.share) + poison_pill + age + 
                                    log(size) + leverage + mtb + roa + short_term +
                                    log_top5_percent + surv_act_appearance_number +
                                    factor(holder_type) + year,
                                    data = data, 
                                    family = "binomial"
            )
se3  = sqrt(diag(  cluster.vcov(reg3, data$type1) ) )
coeftest( reg3, vcov. = cluster.vcov( reg3, data$type1 ) )

reg4 <- glm( success_of_stated_obj ~ log_top5_w_norm_spr + exit_s_board + exit_s_proxy +
                                      log(active.activist.share) + poison_pill + age + 
                                      log(size) + leverage + mtb + roa + short_term +
                                      log_top5_percent + surv_act_appearance_number +
                                      factor(holder_type) + year,
                                      data = data, 
                                      family = "binomial"
)
se4  = sqrt(diag(  cluster.vcov(reg4, data$type1) ) )
coeftest( reg4, vcov. = cluster.vcov( reg4, data$type1 ) )

robust_logit = capture.output(stargazer(reg1, reg2, reg3, reg4,
                                       type = "latex",
                                       title = "Robustness: logistic regression of network support and activist's success",
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
                                       add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes", "Yes"),
                                                        c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                       omit = c("holder_type", "year", ":"),
                                       # omit.labels = "Firm specific controls?",
                                       se=list( se1, se2, se3, se4 ),
                                       table.placement = "H" 
                                       ))

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} Logit regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands.  \\textit{Simple con. weighted by perc.} corresponds to the simple strength of connection weighted by large investor's ownership in the target and aggregated across large investors.
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\textit{Simple con. weighted by import.}  corresponds to the simple strength of connection weighted the importance of target to large investors and aggregated across large investors. \\textit{RI con. weighted by import.} corresponds to the relative influence strength of connection weighted the importance of target to large investors and aggregated across large investors. Importance is defined as $ \\frac{Share - min\\{Share\\}}{\\max\\{Share\\} - \\min\\{Share\\}}$.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01  }} \\\\"
robust_logit[grepl("Note",robust_logit)] <- note.latex

writeLines( robust_logit, "D:/Dropbox/Activist paper/Analysis/new_code/tables/robust_logit.tex" )


# Other
#-------------------------------------------------------------------------
# OLS, but importance defined differently.
#-------------------------------------------------------------------------
#Importance defined by how far is the share of the given firm stands away from the mean share


test2 = short.data.compust[ , c("campaign.id", "success_of_stated_obj" , "log_top5_number", "log_top10_share_nw_s",
                               "log_top5_percent", "log_top5_share_nw_s", "log_top5_share_nw_spr", "log_top5_w_norm_s",
                               "log_top5_w_norm_spr", "log_top5_s_clos_inv", "top5_share_nw_s",
                               "log_top5_sp_clos_inv", "log_top5_s_betw_inv", "log_top5_sp_betw_inv",
                               "log_active.activist_norm_weight", "log_top5_perc",
                               "log_act_num_con", "active.activist.share", "exit_s_board",
                               "exit_s_proxy", "poison_pill",  "age", "size", "leverage", "mtb", "roa",
                               "short_term", "surv_act_appearance_number",  "year", "holder_type", "type1",
                               "log_top10_share_nw_s", "log_top10_percent", "log_top10_share_nw_spr", "log_top10_w_norm_s",
                               "log_top10_w_norm_spr", "log_top20_share_nw_s","log_top20_share_nw_spr", "log_top20_w_norm_s"
                               , "log_top10_w_sd_s", "log_active.activist_sd_weight"
                               # ,"log_top20_w_norm_spr", "log_top5_perc_nw_s", "log_top20_w_sd_s", "log_top5_w_sd_s" 
                               # "log_top20_w_sd_spr", "log_top10_w_sd_spr" ,"log_top5_w_sd_spr", "won_board_ind"
) ]

complete2 = test2[ complete.cases(test2) ]
complete2 = unique(complete2)

data2 = complete2

reg1 <- felm( success_of_stated_obj ~ log_top10_w_sd_s  + exit_s_board + exit_s_proxy + 
                log_active.activist_sd_weight + 
                # log_active.activist_sd_weight*exit_s_board +log_active.activist_sd_weight*exit_s_proxy + 
                poison_pill + age + log(size) + leverage + mtb + surv_act_appearance_number   | holder_type | 0 | type1  , 
                                      data = data2 )

summary(reg1)


test3 = short.data.compust[ , c("campaign.id", "success_of_stated_obj" , "log_top5_number", "log_top10_share_nw_s",
                                "log_top5_percent", "log_top5_share_nw_s", "log_top5_share_nw_spr", "log_top5_w_norm_s",
                                "log_top5_w_norm_spr", "log_top5_s_clos_inv", "top5_share_nw_s",
                                "log_top5_sp_clos_inv", "log_top5_s_betw_inv", "log_top5_sp_betw_inv",
                                "log_active.activist_norm_weight", "log_top5_perc",
                                "log_act_num_con", "active.activist.share", "exit_s_board",
                                "exit_s_proxy", "poison_pill",  "age", "size", "leverage", "mtb", "roa",
                                "short_term", "surv_act_appearance_number",  "year", "holder_type", "type1",
                                "log_top10_share_nw_s", "log_top10_percent", "log_top10_share_nw_spr", "log_top10_w_norm_s",
                                "log_top10_w_norm_spr", "log_top20_share_nw_s","log_top20_share_nw_spr", "log_top20_w_norm_s"
                                , "log_top10_w_sd_spr", "log_active.activist_sd_weight"
                                # ,"log_top20_w_norm_spr", "log_top5_perc_nw_s", "log_top20_w_sd_s", "log_top5_w_sd_s" 
                                # "log_top20_w_sd_spr", "log_top10_w_sd_spr" ,"log_top5_w_sd_spr", "won_board_ind"
) ]

complete3 = test3[ complete.cases(test3) ]
complete3 = unique(complete3)

data3 = complete3

reg2 <- felm( success_of_stated_obj ~ log_top10_w_sd_spr + exit_s_board + exit_s_proxy + 
                log_active.activist_sd_weight + 
                # log_active.activist_sd_weight*exit_s_board +log_active.activist_sd_weight*exit_s_proxy + 
                poison_pill + age + log(size) + leverage + mtb + surv_act_appearance_number  | holder_type | 0 | type1   , 
              data = data3 )

summary(reg2)


robust_importance = capture.output(stargazer( reg1, reg2,
                                       type = "latex",  
                                       title = "Robustness: Network support and activist's success. Importance defined by how far is the share of the given firm stands away from the mean share",
                                       font.size = "tiny", float.env="table",header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       dep.var.labels   = c("Success of stated goals"),
                                       covariate.labels = c("Simple con. weighted by import.", "RI con. weighted by import.",
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
                                       add.lines = list(c("Activist type FE ", "Yes", "Yes"),
                                                        c("Year FE", "Yes", "Yes", "Yes")),
                                       omit=c(":"),
                                       report = "vct*",
                                       digits = 4,
                                       table.placement = "H" 
                                       ))

note.latex <- "\\multicolumn{3}{l} {\\parbox[t]{10cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands.  \\textit{Simple con. weighted by perc.} corresponds to the simple strength of connection weighted by large investor's ownership in the target and aggregated across large investors.
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\textit{Simple con. weighted by import.}  corresponds to the simple strength of connection weighted the importance of target to large investors and aggregated across large investors. \\textit{RI con. weighted by import.} corresponds to the relative influence strength of connection weighted the importance of target to large investors and aggregated across large investors. Importance is defined as $ \\frac{Share - min\\{Share\\}}{\\max\\{Share\\} - \\min\\{Share\\}}$.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01  }} \\\\"
robust_importance[grepl("Note",robust_importance)] <- note.latex

writeLines( robust_importance, "D:/Dropbox/Activist paper/Analysis/new_code/tables/robust_importance.tex" )


#-------------------------------------------------------------------------
# Table 13. NUmber of large investors and activists' success: won_board_ind instead of success of stated obj
#-------------------------------------------------------------------------


test4 = short.data.compust[ , c("campaign.id", "success_of_stated_obj" , "log_top5_number", "log_top10_share_nw_s",
                                "log_top5_percent", "log_top5_share_nw_s", "log_top5_share_nw_spr", "log_top5_w_norm_s",
                                "log_top5_w_norm_spr", "log_top5_s_clos_inv", "top5_share_nw_s",
                                "log_top5_sp_clos_inv", "log_top5_s_betw_inv", "log_top5_sp_betw_inv",
                                "log_active.activist_norm_weight", "log_top5_perc",
                                "log_act_num_con", "active.activist.share", "exit_s_board",
                                "exit_s_proxy", "poison_pill",  "age", "size", "leverage", "mtb", "roa",
                                "short_term", "surv_act_appearance_number",  "year", "holder_type", "type1",
                                "log_top10_share_nw_s", "log_top10_percent", "log_top10_share_nw_spr", "log_top10_w_norm_s",
                                "log_top10_w_norm_spr", "log_top20_share_nw_s","log_top20_share_nw_spr", "log_top20_w_norm_s"
                                , "won_board_ind"
                                # ,"log_top20_w_norm_spr", "log_top5_perc_nw_s", "log_top20_w_sd_s", "log_top5_w_sd_s" 
                                # ,"log_active.activist_sd_weight"
                                # "log_top20_w_sd_spr", "log_top10_w_sd_spr" ,"log_top5_w_sd_spr", "won_board_ind"
) ]

complete4 = test4[ complete.cases(test4) ]
complete4 = unique(complete4)

data4 = complete4

reg1 <- felm( won_board_ind ~ log_top5_number + exit_s_board + exit_s_proxy +
                                      log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                                      mtb + roa + short_term  + surv_act_appearance_number + year | holder_type | 0 | type1  , 
                                      data = data4 )

summary(reg1)

reg2 <- felm( won_board_ind ~ log_act_num_con + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term  + surv_act_appearance_number + year | holder_type | 0 | type1  , 
              data = data4 )

summary(reg2)

reg3 <- felm( won_board_ind ~ log_top5_number + log_act_num_con + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term  + surv_act_appearance_number + year | holder_type | 0 | type1  , 
              data = data4 )

summary(reg3)

reg4 <- felm( won_board_ind ~ log_top5_percent  + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term  + surv_act_appearance_number + year | holder_type | 0 | type1  , 
              data = data4 )

summary(reg4)

reg5 <- felm( won_board_ind ~ log_top5_percent + log_act_num_con + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + surv_act_appearance_number + year | holder_type | 0 | type1  , 
              data = data4 )

summary(reg5)


robust_number = capture.output(stargazer(reg1, reg2, reg3, 
                                       reg4, reg5,
                                       # reg6,reg7,reg8,
                                       type = "latex",  
                                       title = "Robustness: number of large investors and activist's success",
                                       dep.var.labels   = c("Won board seat"),
                                       font.size = "tiny", float.env="table",header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       report = "vct*",
                                       digits = 4,
                                       covariate.labels = c("Num.large investors", "Num. activist connections",
                                                            "Perc. large ownership","Exit after board demands",
                                                            "Exit after proxy fight", "Perc. activist ownership", "Poison pill", "Firm age",
                                                            "log(Market capitalization)", "Leverage", "MTB", "ROA", "Short term objective", "Num. campaigns by activist"
                                                            #, "Exit after board demands x Perc. activist ownership",
                                                            # " Exit after proxy fight x Perc. activist ownership"
                                       ),
                                       add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                        c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                       omit = c("year", ":"),
                                       # omit.labels = "Firm specific controls?",
                                       table.placement = "H" ))

note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{15cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{Won\\_board\\_seat} is an indicator that the activist won at least one board seat.   \\textit{Num. large investors} is the number of investors that fall into top 5\\% holding percentile and have shares in the target. \\textit{Num. activist connections} is total number of connections that activist has with large investors in the target.  \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01 }} \\\\"
robust_number[grepl("Note",robust_number)] <- note.latex
writeLines( robust_number, "D:/Dropbox/Activist paper/Analysis/new_code/tables/robust_number.tex" )

#-------------------------------------------------------------------------
# Table 14: Robustness: network support and activist's success
#-------------------------------------------------------------------------

reg1 <- felm( won_board_ind ~ log_top5_share_nw_s + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
                data = data4 )

summary(reg1)

reg2 <- felm( won_board_ind ~ log_top5_share_nw_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data4 )

summary(reg2)


reg3 <- felm( won_board_ind ~ log_top5_w_norm_s + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data4 )

summary(reg3)

reg4 <- felm( won_board_ind ~ log_top5_w_norm_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data4 )

summary(reg4)


table14 = capture.output(stargazer(reg1, reg2, reg3, reg4,
                                       type = "latex",
                                       title = "Robustness: network support and activist's success",
                                       dep.var.labels   = c("Won board seat"),
                                       font.size = "tiny", float.env="table",header = FALSE,
                                       omit.stat=c("f", "ser"),
                                       report = "vct*",
                                       digits = 4,
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
                                       add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes", "Yes"),
                                                        c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                       omit = c("holder_type", "year")
                                       # omit.labels = "Firm specific controls?",
                                      ))

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{Won\\_board\\_seat} is an indicator that the activist won at least one board seat.  \\textit{Simple con. weighted by perc.} corresponds to the simple strength of connection weighted by large investor's ownership in the target and aggregated across large investors.
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\textit{Simple con. weighted by import.}  corresponds to the simple strength of connection weighted the importance of target to large investors and aggregated across large investors. \\textit{RI con. weighted by import.} corresponds to the relative influence strength of connection weighted the importance of target to large investors and aggregated across large investors. Importance is defined as $ \\frac{Share - min\\{Share\\}}{\\max\\{Share\\} - \\min\\{Share\\}}$.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01  }} \\\\"
table14[grepl("Note",table14)] <- note.latex
writeLines( table14, "D:/Dropbox/Activist paper/Analysis/new_code/tables/table14.tex" )

#-------------------------------------------------------------------------
# Table 15: Robustness: Activists' centrality and activist's success
#-------------------------------------------------------------------------

reg1 <- felm( won_board_ind ~ log_top5_s_clos_inv + top5_share_nw_s + exit_s_board + exit_s_proxy +
                log_active.activist_norm_weight +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term  + surv_act_appearance_number | holder_type | 0 | type1  , 
                data = data4 )

summary(reg1)

# reg2 <- felm( won_board_ind ~ log_top5_spr_clos_inv + top5_share_nw_s + exit_s_board + exit_s_proxy +
#                 log_active.activist_norm_weight +  poison_pill + age + log(size) + leverage + 
#                 mtb + roa + short_term  + surv_act_appearance_number | holder_type | 0 | type1  , 
#               data = data4 )
# 
# summary(reg2)

reg3 <- felm( won_board_ind ~ log_top5_s_betw_inv + top5_share_nw_s + exit_s_board + exit_s_proxy +
                log_active.activist_norm_weight +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term  + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data4 )

summary(reg3)

reg4 <- felm( won_board_ind ~ log_top5_sp_betw_inv + top5_share_nw_s + exit_s_board + exit_s_proxy +
                log_active.activist_norm_weight +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term  + surv_act_appearance_number | holder_type | 0 | type1  , 
              data = data4 )

summary(reg4)


robust_centr = capture.output(stargazer(reg1, #reg2,
                                        reg3, reg4,
                                 type = "latex",
                                 dep.var.labels   = c("Won board seat"),
                                 title = "Robustness: Activists' centrality and activist's success",
                                 covariate.labels = c("Closeness centrality", #"RI closeness centrality",
                                                      "Simple betweenness centrality", "RI betweenness centrality",
                                                      "RI con. weighted by perc.",
                                                      "Import. activist ownership",
                                                      "Exit after board demands",
                                                      "Exit after proxy fight", 
                                                      "Poison pill", "Firm age",
                                                      "log(Market capitalization)", "Leverage", "MTB", "ROA", "Short term objective", "Num. campaigns by activist"),
                                 add.lines = list(c("Activist type FE ", "Yes", "Yes", "Yes"),
                                                  c("Year FE", "Yes", "Yes", "Yes", "Yes")), 
                                 font.size = "tiny", float.env="table",header = FALSE,
                                 omit.stat=c("f", "ser"),
                                 omit = c("year"), 
                                 report = "vct*",
                                 digits = 4
                               ))

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{11cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha + \\beta_1Investor\\_importance+ \\beta_2Network\\_support + \\beta_3Activist\\_power + \\beta_4Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands. \\textit{Simple closeness centrality} is an aggregate closeness centrality computed with simple network. \\textit{RI closeness centrality} is an aggregate closeness centrality computed with relative influence network. \\textit{Simple betweennes centrality} is an aggregate betweennes centrality computed with simple network. \\textit{RI betweenness centrality} is an aggregate betweennes centrality computed with relative influence network. 
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company. \\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01 }} \\\\"
robust_centr[grepl("Note",robust_centr)] <- note.latex
writeLines( robust_centr, "D:/Dropbox/Activist paper/Analysis/new_code/tables/robust_centr.tex" )


#-------------------------------------------------------------------------
# Table 16: Robustness: Network support and activist's success. Campaign type FE instead of activists type FE
#-------------------------------------------------------------------------


reg1 <- felm( success_of_stated_obj ~ log_top5_share_nw_s + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | activist_obj_type_1  , 
              data = data )

summary(reg1)


reg2 <- felm( success_of_stated_obj ~ log_top5_share_nw_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | activist_obj_type_1  , 
              data = data )

summary(reg2)

reg3 <- felm( success_of_stated_obj ~ log_top5_w_norm_s + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | activist_obj_type_1  , 
              data = data )

summary(reg3)


reg4 <- felm( success_of_stated_obj ~ log_top5_w_norm_spr + exit_s_board + exit_s_proxy +
                log(active.activist.share) +  poison_pill + age + log(size) + leverage + 
                mtb + roa + short_term + log_top10_percent + surv_act_appearance_number | holder_type | 0 | activist_obj_type_1  , 
              data = data )

summary(reg4)


camp_type_clustering = capture.output(stargazer(reg1, reg2, reg3, reg4,
                                       # reg5,reg6, reg7,reg8,
                                       type = "latex",
                                       title = "Robustness: Network support and activist's success. Campaign type FE instead of activists type FE",
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
                                       add.lines = list(c("Campaign type FE ", "Yes", "Yes", "Yes", "Yes"),
                                                        c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                       omit = c("activist_obj_type_1", "year", ":")
                                 
))

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} OLS regression  of the equation $Campaign\\_outcome = \\alpha+ \\beta_1Network\\_support + \\beta_2Activist\\_power + \\beta_3Activist\\_cost + Controls + e$.
\\textit{success\\_of\\_stated\\_goals} is an indicator of fulfillment of activists' demands.  \\textit{Simple con. weighted by perc.} corresponds to the simple strength of connection weighted by large investor's ownership in the target and aggregated across large investors.
\\textit{RI con. weighted by perc.} corresponds to the relative influence strength of connection weighted by large investor's ownership in the target and aggregated across large investors. \\textit{Simple con. weighted by import.}  corresponds to the simple strength of connection weighted the importance of target to large investors and aggregated across large investors. \\textit{RI con. weighted by import.} corresponds to the relative influence strength of connection weighted the importance of target to large investors and aggregated across large investors. Importance is defined as $ \\frac{Share - min\\{Share\\}}{\\max\\{Share\\} - \\min\\{Share\\}}$.  \\textit{Perc. activist ownership} corresponds to the aggregate percentage owned by activists. \\textit{Exit after board demands} is a dummy which is equal to one if the campaign ends with activist's demands and does not go to proxy fight. \\textit{Exit after proxy fight} is an indicator that proxy fight happened over the campaign.  \\textit{Poison pill} is an indicator that the firm had poison pill prior to the campaign or adopted it in response to the campaign.  \\textit{Firm age} is the age of the target. \\textit{Market capitalization} is market capitalization of the target at the start of campaign. \\textit{Leverage} is the long-term leverage of the target firm. \\textit{MTB} is market-to-book value of the target firm at the start of the campaign. \\textit{ROA} is return-on-assets of the target at the start of the campaign. \\textit{Perc. large ownership} is aggregate percentage of the target owned by large investors. \\textit{Short term objective} is a dummy equal to one if activist's demands include payout of divideds, share repurchase programs or sale of the company.\\textit{Num. campaigns by activist} is total number of campaigns that activist was involved in the sample. Each regression contains year fixed effects and an campaign type fixed effects. Robust standard errors are clustered by the campaign type (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01  }} \\\\"
camp_type_clustering[grepl("Note",camp_type_clustering)] <- note.latex
writeLines( camp_type_clustering, "D:/Dropbox/Activist paper/Analysis/new_code/tables/camp_type_clustering.tex" )


#-------------------------------------------------------------------------
# Table 17: Robustness: Activist's success and connection strength. Benchmark against activist herself when not active
#-------------------------------------------------------------------------

load("winlose_self_control")

did_30_self = felm(diff_number ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company |activist_cik+year| 0 | 0 ,
                   data = winlose )

summary( did_30_self )

did_31_self = felm(diff_s ~ treated+win+in_same_company+treated*win+treated*in_same_company+treated*win*in_same_company |activist_cik+year| 0 | 0  ,
                   data = winlose )

summary( did_31_self )

did_out = capture.output(stargazer(  did_30_self, did_31_self,
                                     type = "latex",
                                     title = "Robustness: Activist's success and connection strength. Benchmark against activist herself when not active.",
                                     font.size = "footnotesize",
                                     float.env="table",header = FALSE,
                                     omit.stat=c("f", "ser"),
                                     report = "vct*",
                                     digits = 4,
                                     omit = c("activist_cik", "year"),
                                     add.lines = list(c("Activist FE ", "Yes", "Yes", "Yes", "Yes"),
                                                      c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
                                     covariate.labels = c("Connected to active activist", "Activist wins",
                                                          "Target shareholder", "Connected to active activist x Target shareholder",
                                                          "Connected to active activist x Activist wins",
                                                          "Connected to active activist x Target shareholder",
                                                          "Connected to active activist x Target shareholder x Activist wins"),
                                     dep.var.labels   = c("$\\Delta(Simple)$", "$\\Delta(RI)$"),
                                     # omit.yes.no = T,
                                     # omit.labels = "Firm specific controls?",
                                     single.row = TRUE
                                     # ,se=list(se_30, se_31 )
))


note.latex <- "\\multicolumn{3}{l} {\\parbox[t]{17cm}{ \\textit{Notes:} OLS regression  of the equation \\\\	$	\\Delta(Connection\\_strength) = \\alpha +\\beta_1 Connected\\_to\\_active\\_activist  +\\beta_2 Activist\\_wins + \\beta_3Target\\_shareholder + \\gamma_1Connected\\_to\\_active\\_activist \\cdot Activist\\_wins+ \\gamma_2 Connected\\_to\\_active\\_activist \\cdot Target\\_shareholder+ \\gamma_3 Activist\\_wins \\cdot Target\\_shareholder+ \\delta Connected\\_to\\_active\\_activist \\cdot Activist\\_wins \\cdot Target\\_shareholder+Controls +\\epsilon $. In the first column, 	$\\Delta(Simple)$ corresponds to the change in the simple strength (number of connections). In the second column, $\\Delta(RI)$ corresponds to the change in the relative influence strength. $Connected\\_to\\_active\\_activist$ is an indicator that passive investor is connected to the activist in active stage, $Activist\\_wins$ is a dummy of activist's victory, and $Target\\_shareholder$ is an indicator that passive investor is a shareholder of the target company. Each regression contains year fixed effects and an activist's type (e.g. hedge fund, pension fund, etc.) fixed effects. Robust standard errors are clustered by the campaign category (capital structure, business strategy, sell company, governance or other). Table contains standard significance levels: *p<0.1; **p<0.05, ***p<0.01 }} \\\\"
did_out[grepl("Note",did_out)] <- note.latex
writeLines( did_out, "D:/Dropbox/Activist paper/Analysis/new_code/tables/robust_did.tex" )
