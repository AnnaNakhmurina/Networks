rm(list=ls())

library(sandwich)
library(stargazer)
library(lmtest)
setwd("~/Networks")

load("short.data.compust")


# Test on covariences and stuff
# Run simplest regresion

plot(short.data.compust$total.activist.number,short.data.compust$won_brep_dummy)

#-------------------------------------------RUN TYPE 1 REGRESSIONS -------------------------------------------------------------------

# Run LOGIT regressions of type ONE -- with heter robust se.
# !!!!!! Clustered se -- cluster by campaign type? 

# So far, I cannot control for iss/glass lewis support because I only have 14 obersations where support is known. NEED TO ADD IT LATER 

#THIS ONE!!!!
logit_1_binar <- glm(won_brep_dummy ~ log(active.activist.size)+total.activist.number, data = short.data.compust, family = "binomial")
coeftest(logit_1_binar, vcov. = vcovHC)
robust_se_1_binar    <- sqrt(diag( vcovHC(logit_1_binar)))

logit_1_binar_investor <- glm(won_brep_dummy ~ log(active.activist.size)+investor.number, data = short.data.compust, family = "binomial")
coeftest(logit_1_binar_investor, vcov. = vcovHC)
robust_se_1_binar_investor    <- sqrt(diag(vcovHC(logit_1_binar_investor) ))


# NEGATIVE 10% SIGNIFICANT
logit_1_classified <- glm(success_of_stated_obj ~ log(active.activist.size)+(total.activist.number), data = short.data.compust, family = "binomial")
coeftest(logit_1_classified, vcov. = vcovHC)
robust_se_1_classified   <- sqrt(diag( vcovHC(logit_1_classified) ))


logit_1_classified_investor <- glm(success_of_stated_obj ~log(active.activist.size)+(investor.number), data = short.data.compust, family = "binomial")
coeftest(logit_1_classified_investor, vcov. = vcovHC)
robust_se_1_classified_investor   <- sqrt(diag( vcovHC(logit_1_classified_investor) ))

# FIRST STARGAZER OUTPUT IS HERE

logit1_output= stargazer(logit_1_binar, logit_1_binar_investor,logit_1_classified, logit_1_classified_investor,
                        type = "latex",  title = "Logit regressions with robust se",
                        se=list(robust_se_1_binar,robust_se_1_binar_investor,robust_se_1_classified,robust_se_1_classified_investor))

# Run OLS regressions of type ONE -- with heter robust se's 

ols1_binar <- lm(won_brep_dummy ~ log(active.activist.size)+total.activist.number, data = short.data.compust)
coeftest(ols1_binar, vcov. = vcovHC)
robust_se_ols1_binar   <- sqrt(diag( vcovHC(ols1_binar) ))

ols1_binar_investor <- lm(won_brep_dummy ~ log(active.activist.size)+investor.number, data = short.data.compust)
coeftest(ols1_binar_investor, vcov. = vcovHC)
robust_se_ols1_binar_investor   <- sqrt(diag( vcovHC(ols1_binar_investor) ))


ols1_classified <- lm(success_of_stated_obj ~ log(active.activist.size)+total.activist.number, data = short.data.compust)
coeftest(ols1_classified, vcov. = vcovHC)
robust_se_ols1_classified   <- sqrt(diag( vcovHC(ols1_classified) ))


ols1_classified_investor <- lm(success_of_stated_obj ~ log(active.activist.size)+investor.number, data = short.data.compust)
coeftest(ols1_classified_investor, vcov. = vcovHC)
robust_se_ols1_classified_investor   <- sqrt(diag(vcovHC(ols1_classified_investor) ))

# Add controls

ols1_binar_controls <- lm(won_brep_dummy ~ log(active.activist.size)+total.activist.number+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols1_binar_controls, vcov. = vcovHC)
robust_se_ols1_binar_controls   <- sqrt(diag( vcovHC(ols1_binar_controls) ))

ols1_classified_controls <- lm(success_of_stated_obj ~ log(active.activist.size)+total.activist.number+size, data = short.data.compust)
coeftest(ols1_classified_controls, vcov. = vcovHC)
robust_se_ols1_classified_controls   <- sqrt(diag( vcovHC(ols1_classified_controls) ))

# SECOND STARGAZER OUTPUT IS HERE


ols1_output = stargazer(ols1_binar,ols1_binar_investor,ols1_classified, ols1_classified_investor, ols1_binar_controls,ols1_classified_controls,
          type = "latex",  title = "OLS regressions with robust se", font.size = "footnotesize", float.env="sidewaystable",
          se=list(robust_se_ols1_binar,robust_se_ols1_binar_investor,robust_se_ols1_classified,
                  robust_se_ols1_classified_investor,robust_se_ols1_binar_controls,robust_se_ols1_classified_controls))


# Sales and operational profitab growth as outcome variables

ols1_sales <- lm(sales_growth ~ log(active.activist.size)+total.activist.number, data = short.data.compust)
coeftest(ols1_sales, vcov. = vcovHC)
robust_se_ols1_sales   <- sqrt(diag( vcovHC(ols1_sales) ))

ols1_sales_controls <- lm(sales_growth ~ log(active.activist.size)+total.activist.number+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols1_sales_controls, vcov. = vcovHC)
robust_se_ols1_sales_controls   <- sqrt(diag( vcovHC(ols1_sales_controls) ))


ols1_operat <- lm(oper_profit_growth ~ log(active.activist.size)+total.activist.number, data = short.data.compust)
coeftest(ols1_operat, vcov. = vcovHC)
robust_se_ols1_operat    <- sqrt(diag( vcovHC(ols1_operat) ))


ols1_operat_controls <- lm(oper_profit_growth ~ log(active.activist.size)+total.activist.number+total.activist.number+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols1_operat_controls, vcov. = vcovHC)
robust_se_ols1_operat_controls    <- sqrt(diag( vcovHC(ols1_operat_controls) ))

# THIRD STARGAZER OUTPUT IS HERE

ols1_output_operat = stargazer(ols1_sales, ols1_sales_controls, ols1_operat,ols1_operat_controls,
          type = "latex",  title = "OLS regressions with robust se,  operational outcome variables", 
          se=list(robust_se_ols1_sales, robust_se_ols1_sales_controls, robust_se_ols1_operat,robust_se_ols1_operat_controls))



#-------------------------------------------RUN TYPE 2 REGRESSIONS -------------------------------------------------------------------

# Average

ols_2_binar_av <- glm(won_brep_dummy ~ log(active.activist.size)+log(activist.size.average), data = short.data.compust, family = "binomial")
coeftest(ols_2_binar_av, vcov. = vcovHC)
robust_se_ols_2_binar_av    <- sqrt(diag( vcovHC(ols_2_binar_av) ))


ols_2_binar_controls_av <- lm(won_brep_dummy ~ log(active.activist.size)+log(activist.size.average)+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols_2_binar_controls_av, vcov. = vcovHC)
robust_se_ols_2_binar_controls_av   <- sqrt(diag( vcovHC(ols_2_binar_controls_av) ))


ols_2_classified_av <- glm(success_of_stated_obj ~ log(active.activist.size)+log(activist.size.average), data = short.data.compust, family = "binomial")
coeftest(ols_2_classified_av, vcov. = vcovHC)
robust_se_ols_2_classified_av   <- sqrt(diag( vcovHC(ols_2_classified_av) ))


ols_2_classified_controls_av <- lm(success_of_stated_obj ~ log(active.activist.size)+log(activist.size.average)+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols_2_classified_controls_av, vcov. = vcovHC)
robust_se_ols_2_classified_controls_av   <- sqrt(diag( vcovHC(ols_2_classified_controls_av) ))


# Value weighted

ols_2_binar <- glm(won_brep_dummy ~ log(active.activist.size)+log(activist.size.vweighted), data = short.data.compust, family = "binomial")
coeftest(ols_2_binar, vcov. = vcovHC)
robust_se_ols_2_binar   <- sqrt(diag( vcovHC(ols_2_binar) ))

ols_2_binar_controls <- lm(won_brep_dummy ~ log(active.activist.size)+log(activist.size.vweighted)+age+size+leverage+mtb, data = short.data.compust)
vcovHC(ols_2_binar_controls)
coeftest(ols_2_binar_controls, vcov. = vcovHC)
robust_se_ols_2_binar_controls   <- sqrt(diag( vcovHC(ols_2_binar_controls) ))


ols_2_classified <- glm(success_of_stated_obj ~ log(active.activist.size)+log(activist.size.vweighted), data = short.data.compust, family = "binomial")
coeftest(ols_2_classified, vcov. = vcovHC)
robust_se_ols_2_classified  <- sqrt(diag( vcovHC(ols_2_classified) ))


ols_2_classified_controls <- lm(success_of_stated_obj ~ log(active.activist.size)+log(activist.size.vweighted)+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols_2_classified_controls, vcov. = vcovHC)
robust_se_ols_2_classified_controls <- sqrt(diag( vcovHC(ols_2_classified_controls) ))


# FOURTH STARGAZER OUTPUT IS HERE
ols2_output =stargazer(ols_2_binar_av, ols_2_binar_controls_av,ols_2_classified_av,ols_2_classified_controls_av,ols_2_binar,ols_2_binar_controls,
          ols_2_classified, ols_2_classified_controls,
          type = "latex",  title = "Basic spillower OLS regressions with robust se",font.size = "footnotesize", float.env="sidewaystable",
          se=list(robust_se_ols_2_binar_av,robust_se_ols_2_binar_controls_av,robust_se_ols_2_classified_av,
                  robust_se_ols_2_classified_controls_av,robust_se_ols_2_binar,robust_se_ols_2_binar_controls, 
                  robust_se_ols_2_classified,robust_se_ols_2_classified_controls))

# With operational outcomes

ols2_sales <- lm(sales_growth ~ log(active.activist.size)+log(activist.size.vweighted), data = short.data.compust)
coeftest(ols2_sales, vcov. = vcovHC)
robust_se_ols2_sales <- sqrt(diag( vcovHC(ols2_sales) ))

ols2_sales_controls <- lm(sales_growth ~ log(active.activist.size)+log(activist.size.vweighted)+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols2_sales_controls, vcov. = vcovHC)
robust_se_ols2_sales_controls <- sqrt(diag( vcovHC(ols2_sales_controls) ))


ols_2_oper<- lm(oper_profit_growth ~ log(active.activist.size)+log(activist.size.vweighted), data = short.data.compust)
coeftest(ols_2_oper, vcov. = vcovHC)
robust_se_ols_2_oper <- sqrt(diag( vcovHC(ols_2_oper) ))


ols_2_oper_controls <- lm(oper_profit_growth ~ log(active.activist.size)+log(activist.size.vweighted)+age+size+leverage+mtb, data = short.data.compust)
coeftest(ols_2_oper_controls, vcov. = vcovHC)
robust_se_ols_2_oper_controls <- sqrt(diag( vcovHC(ols_2_oper_controls) ))

# FIFTH STARGAZER OUTPUT IS HERE
ols2_output_oper= stargazer(ols2_sales, ols2_sales_controls,ols_2_oper,ols_2_oper_controls,
          type = "latex",  title = "Basic spillower OLS regressions with robust se, operational outcome variables",
          se=list(robust_se_ols2_sales,robust_se_ols2_sales_controls,robust_se_ols_2_oper,robust_se_ols_2_oper_controls))

#-------------------RUN TYPE 3 REGRESSIONS -- WITH NETWORKS CENTRALITY

ols3_binar_sic <- lm(won_brep_dummy ~ act_simple_closeness+oth_simple_closeness, data = short.data.compust)
coeftest(ols3_binar_sic, vcov. = vcovHC)
re_ols3_binar_sic   <- sqrt(diag( vcovHC(ols3_binar_sic) ))

ols3_binar_sibe <- lm(won_brep_dummy ~ act_simple_betweennes+oth_simple_betweennes, data = short.data.compust)
coeftest(ols3_binar_sibe, vcov. = vcovHC)
re_ols3_binar_sibe   <- sqrt(diag( vcovHC(ols3_binar_sibe) ))

ols3_binar_sibo <- lm(won_brep_dummy ~ act_simple_bonacich+oth_simple_bonacich, data = short.data.compust)
coeftest(ols3_binar_sibo, vcov. = vcovHC)
re_ols3_binar_sibo   <- sqrt(diag( vcovHC(ols3_binar_sibo) ))

ols3_binar_spc <- lm(won_brep_dummy ~ act_spring_closeness+oth_spring_closeness, data = short.data.compust)
coeftest(ols3_binar_spc, vcov. = vcovHC)
re_ols3_binar_spc   <- sqrt(diag( vcovHC(ols3_binar_spc) ))

ols3_binar_spbe <- lm(won_brep_dummy ~ act_spring_betweennes+oth_spring_betweennes, data = short.data.compust)
coeftest(ols3_binar_spbe, vcov. = vcovHC)
re_ols3_binar_spbe  <- sqrt(diag( vcovHC(ols3_binar_spbe) ))

ols3_binar_sbo <- lm(won_brep_dummy ~ act_spring_bonacich+oth_spring_bonacich, data = short.data.compust)
coeftest(ols3_binar_sbo, vcov. = vcovHC)
re_ols3_binar_sbe   <- sqrt(diag( vcovHC(ols3_binar_sbo) ))