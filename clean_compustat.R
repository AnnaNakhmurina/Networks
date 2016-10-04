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

setwd("~/Networks")
compustat <- read.csv("compustat_for_age.csv")
compustat$cusip6 <-  substr(compustat$cusip, 1,6)

compustat_firstdate <- aggregate( compustat[c("datadate")], compustat[c("cusip6")], FUN=min )
names(compustat_firstdate) <- c("cusip6","first_date")


compustat <- read.csv("compustat_q_2015.csv")
compustat$cusip6 <-  substr(compustat$cusip, 1,6)

compustat <- merge(compustat, compustat_firstdate, by=("cusip6"))

compustat$date <- as.Date(as.character(compustat$datadate),"%Y%m%d")
compustat$first_date <- as.Date(as.character(compustat$first_date),"%Y%m%d")

compustat$age = year(compustat$date) - year(compustat$first_date)

save(compustat, file= "compustat_q_2015_w_age")
