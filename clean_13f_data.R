rm(list=ls())
gc()

library(plyr)
library(XML)
library(xml2)
library(RCurl)
library(dplyr)
library(pryr)
library(stringr)
library(data.table)
library(gtools)

# This file creates a network (adjacency) matrix

setwd("~/Networks/Data/13f_test")

temp = list.files(pattern="all_data.")

data_13f <- data.frame()

for (i in 21:22){
  print(temp[i])
file <- get( load(temp[i]) )

data_13f = bind_rows(data_13f, file)

rm(list=setdiff(ls(), c("data_13f","temp", "i")) )

}

data_13f_21_22 = data_13f
save(data_13f_21_22, file="data_13f_21_22")

# Throw away unneeded rows

rm(list=setdiff(ls(), c("data_13f_2014_2015")) )

cols.to.keep = c("infoTable.nameOfIssuer","infoTable.titleOfClass","infoTable.cusip",
                 "infoTable.value","infoTable.shrsOrPrnAmt.sshPrnamt",
                 "infoTable.shrsOrPrnAmt.sshPrnamtType","infoTable.putCall",
                 "infoTable.investmentDiscretion","infoTable.votingAuthority.Sole",
                 "infoTable.votingAuthority.Shared","infoTable.votingAuthority.None",
                 "cik","fund_name","period")

data_13_2014 = data_13_2014[cols.to.keep]

save(data_13f_2014_2015, file="data_13f_2014_2015")

data_13f_2014_2015 <- rbind(data_13f_2014_short, data_13f_2015_short)

# Select only those files which are connected with activists
# 
# 
# setwd("~/Networks")
# load("clean.shark.partial")
# 
# 
# # Select the subsample of data that corresponds to all campaigns that were going of after 
# # the start of 2015
# shark.sub.2015 = clean.shark.partial[!is.na(clean.shark.partial$dissident_board_seats_sought),]
# shark.sub.2015 = shark.sub.2015[(shark.sub.2015$announce_date> "2014-12-31"),]
# 
# out <- shark.sub.2015[c("activist.name","company_name","board_seats_up", "dissident_board_seats_sought",
#                         "dissident_board_seats_won", "dissident_tactic_nominate_slate_of_directors", "outcome", "campaign_status" )]
# 
# # Create a 6-digit cusip
# 
# shark.sub.2015$cusip6 <- substr(shark.sub.2015$cusip_9_digit, 1,6)
# 
# # Put an activists flag 
# 
# shark.sub.2015$active <- "active"
# 
# # Now, download the 13f file
# setwd("~/Networks/Data/13f_test")
# 
# load("cusip_ok.dt")
# 
# cusip_ok.dt <- data.frame(cusip_ok.dt)
# 
# setwd("~/Networks")
# 
# # We are only going to need filings that concern a list of given companies as a first step
# cusip.list <- unique(shark.sub.2015$cusip6)


# Clean this data

data_13f = data_13f_short

# Remove "Infotable."  from names of all the entries

names(data_13f) <- gsub("^.*?infoTable.","", names(data_13f))
names(data_13f) <- gsub("^.*?shrsOrPrnAmt.","", names(data_13f))
names(data_13f) <- gsub("^.*?headerData.","", names(data_13f))
names(data_13f) <- gsub("^.*?formData.coverPage.","", names(data_13f))


save(data_13f, file="data_13f")

# REmove empty entries

data_13f<- data_13f[!(data_13f$nameOfIssuer == "No issuers to report"), ] #filters out 'No issuers to report'
data_13f<- data_13f[!(is.na(data_13f$nameOfIssuer)), ] #filters out NA
data_13f<- data_13f[!(data_13f$nameOfIssuer == "0"), ] #filters out 0
data_13f<- data_13f[!(data_13f$nameOfIssuer == "N/A"), ] #filters out 'N/A'


# Create a datadate variable for consistency
data_13f$datadate <- as.integer( paste0(substring(data_13f$period, 7, 10),substring(data_13f$period, 1, 2),substring(data_13f$period, 4, 5)) )


# Clean the data_13f table -- remove white spaces etc

trim <- function (x) gsub("^\\s+|\\s+$", "", x) #this function trims leading and trailing whitespace
data_13f$cusip <- trim(data_13f$cusip)
data_13f$sshPrnamtType <- trim(data_13f$sshPrnamtType)
data_13f$cusip <- toupper(data_13f$cusip)
data_13f$nameOfIssuer <- toupper(data_13f$nameOfIssuer)

#-------------Get saved (PARTIAL) file --------------

load("data_13f")

data_13f_2014_short$cusip6<- substr(data_13f_2014_short$cusip,1, 6)

#first need to make everything upper case for cusips to align better

cusip_problems <- filter(data_13f, nchar(data_13f$cusip6) < 6)
cusip_ok <- filter(data_13f, nchar(data_13f$cusip) >= 6  )

# Some of the filers mixed up the columns -- fix this

mixed.columns <- filter(cusip_problems, cusip %in% "COM" )
still.problems <- filter(mixed.columns, titleOfClass %in% "COM")
mixed.columns  <- filter(mixed.columns, !(titleOfClass %in% "COM"))
mixed.columns <- mixed.columns[c("titleOfClass", "cusip", 
                                 "nameOfIssuer",
                                 names(mixed.columns)[4:length(names(mixed.columns))] )]
names(mixed.columns) <- names(data_13f)

cusip_ok <- bind_rows(cusip_ok, mixed.columns)


#---------------------START HERE

save(cusip_ok, file="cusip_ok")

# Create the total portfolio value for each fund in each period

load("cusip_ok")

# Add a total value column

cusip_ok.dt <- as.data.table(cusip_ok)
save(cusip_ok.dt, file="cusip_ok.dt")



# cusip_ok.dt$value  =  as.numeric(cusip_ok.dt$value)
cusip_ok.dt <-cusip_ok.dt[, total.value := sum( as.numeric(value) ), by = list(cik, period)]
a = cusip_ok.dt[which(cusip_ok.dt$period == "03-31-2015"& cusip_ok.dt$cik == "1388391")]

rm(list=setdiff(ls(), c("cusip_ok.short")) )

load("cusip_ok.dt")
# Sum over all cusip6 investments to get rid of ambuguity

names= c( "datadate", "cik","fund_name","period","cusip6","total.value" )

dt = as.data.table(cusip_ok.dt)
dt$value <- as.numeric(dt$value)
dt$votingAuthority.Sole <- as.numeric(dt$votingAuthority.Sole)
dt$votingAuthority.Shared <- as.numeric(dt$votingAuthority.Shared)
dt$votingAuthority.None <- as.numeric(dt$votingAuthority.None)
dt$sshPrnamt <- as.numeric(dt$sshPrnamt)
cusip_ok.short= dt[,  list(value.mln.all =sum(value),share.amt =sum(sshPrnamt),votingAuthority.Sole =sum(votingAuthority.Sole), 
                                        votingAuthority.Shared=sum(votingAuthority.Shared),
                                        votingAuthority.None=sum(votingAuthority.None) ), 
                                        by=c(names, "cik")]
save(cusip_ok.short, file="cusip_ok.short")


# cusip_ok.short = cusip_ok.short[,-2]


#------------------------------------ADD CENTRALITY MEASURES TO THE FUND NETWORK DATA--------------------------------------
setwd("~/Networks")
load("cusip_ok.short")


library(igraph)
library(statnet)
library(network)
library(sna)


load("fund_network_wzeros_1q_2015")

centrality_table = function ( fund_network ){

simple = fund_network[c("fund1","fund2")]
connections_number = fund_network[c("fund1","fund2","num_con")]
spring = fund_network[c("fund1","fund2","s")]

net_simple <- graph_from_data_frame(d=simple, directed=F) 
net_connections_number  <- graph_from_data_frame(d=connections_number, directed=F) 
net_spring <- graph_from_data_frame(d=spring, directed=T) 

simple_adjnet = as_adjacency_matrix(net_simple)
con_adjnet = as_adjacency_matrix(net_connections_number)
con_spring = as_adjacency_matrix(net_spring)

simple_am = as.matrix(simple_adjnet)
con_am = as.matrix(simple_adjnet)
spring_am = as.matrix(con_spring)

simple_degree=degree(simple_am)
con_degree = degree(con_am)
spring_degree = degree(spring_am)

names(simple_degree)=names(V(net_simple))
names(con_degree)=names(V(net_connections_number))
names(spring_degree)=names(V(net_spring))

simple_between=betweenness(simple_am)
con_between=betweenness(con_am)
spring_between=betweenness(spring_am)

names(simple_between)=names(V(net_simple))
names(con_between)=names(V(net_connections_number))
names(spring_between)=names(V(net_spring))

simple_clos=closeness(simple_am)
con_clos=closeness(con_am)
spring_clos=closeness(spring_am)

names(simple_clos)=names(V(net_simple))
names(con_clos)=names(V(net_connections_number))
names(spring_clos)=names(V(net_spring))

simple_bonacich=power_centrality(net_simple, nodes = V(net_simple), loops = FALSE, exponent = 1,
                          rescale = FALSE, tol = 1e-07, sparse = TRUE)
con_bonacich=power_centrality(net_connections_number, nodes = V(net_connections_number), loops = FALSE, exponent = 1,
                                 rescale = FALSE, tol = 1e-07, sparse = TRUE)
spring_bonacich=power_centrality(net_spring, nodes = V(net_spring), loops = FALSE, exponent = 1,
                                 rescale = FALSE, tol = 1e-07, sparse = TRUE)


summary = cbind(simple_degree,simple_clos,simple_between,simple_bonacich,
                con_degree,con_between,con_clos,con_bonacich,
                spring_degree,spring_between,spring_clos,spring_bonacich)
summary= as.data.frame(summary)
summary$cik = rownames(summary)
rownames(summary) <- NULL
return(summary)
}

load("fund_network_1q_2015")
summary_1q = centrality_table(fund_network)
summary_1q$period = "03-31-2015"

load("fund_network_2q_2015")
summary_2q = centrality_table(fund_network)
summary_2q$period = "06-30-2015"

load("fund_network_3q_2015")
summary_3q = centrality_table(fund_network)
summary_3q$period = "09-30-2015"

load("fund_network_4q_2015")
summary_4q = centrality_table(fund_network)
summary_4q$period = "12-31-2015"

centrality_summary  = rbind(summary_1q,summary_2q,summary_3q,summary_4q)
 
cusip_ok.short = merge(cusip_ok.short,centrality_summary, by =c("cik", "period"))
save(cusip_ok.short, file="cusip_ok.short")
