# This file cleans the compustat file
rm(list=ls())

library(plyr)
library(XML)
library(xml2)
library(RCurl)
library(dplyr)
library(pryr)
library(stringr)
library(data.table)
library(gtools)
library(lubridate)

setwd("~/Networks/Analysis")
compustat <- read.csv("compustat_for_age.csv")
compustat$cusip6 <-  substr(compustat$cusip, 1,6)

compustat_firstdate <- aggregate( compustat[c("datadate")], compustat[c("cusip6")], FUN=min )
names(compustat_firstdate) <- c("cusip6","first_date")


# compustat <- read.csv("compustat_q_2015.csv")
# compustat <- read.csv("compustat_q_2000_2007.csv")
compustat <- read.csv("compustat_q_2008_2014.csv")
# Select the variables
compustat_short = compustat[c("dlttq", "dlcq", "seqq","mkvaltq",
                              "cshoq", "prccq", "ceqq", "xrdq", "revtq", "cogsq", "xsgaq",
                              "atq","niq","txditcq", "datadate", "cusip")]
save(compustat_short, file="compustat_short_2008_2014")

load("compustat_short_2000_2014")
compustat=compustat_short
rm(compustat_short)

compustat$cusip6 <-  substr(compustat$cusip, 1,6)

compustat <- merge(compustat, compustat_firstdate, by=("cusip6"))

compustat$date <- as.Date(as.character(compustat$datadate),"%Y%m%d")

compustat$first_date <- as.Date(as.character(compustat$first_date),"%Y%m%d")

compustat$age = year(compustat$date) - year(compustat$first_date)

# save(compustat, file= "compustat_q_2015_w_age")

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
compustat$asset_turnover = compustat$revtq/compustat$atq
compustat$rd_to_assets = compustat$xrdq
compustat$period = as.Date(as.character(compustat$datadate),"%Y%m%d")

save(compustat, file="compustat_short_2008_2014")

load("compustat_short_2008_2014")
c=compustat
load("compustat_short_2000_2007")
compustat= rbind(compustat,c)

save(compustat, file="compustat_short_2000_2014")
