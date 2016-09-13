
setwd("~/Networks")
load("clean.shark.partial")

to.classify = clean.shark.partial[c("company_name", "campaign_id",
                                    "dissident_tactic", "outcome", 
                                    "announce_date", "dissident_group")]
to.classify = unique(to.classify)
to.classify.2015 =to.classify[(to.classify$announce_date> "2014-12-31"),]
write.csv(to.classify.2015, file="to.classify.2015.csv")

# # Select the subsample of data that corresponds to all campaigns that were going of after 
# # the start of 2015
# shark.sub.2015 = clean.shark.partial[!is.na(clean.shark.partial$dissident_board_seats_sought),]
# shark.sub.2015 = shark.sub.2015[(shark.sub.2015$announce_date> "2014-12-31"),]
# 
# out <- shark.sub.2015[c("activist.name","company_name","board_seats_up", "dissident_board_seats_sought",
#         "dissident_board_seats_won", "dissident_tactic_nominate_slate_of_directors", "outcome", "campaign_status" )]
# 
# # Create a 6-digit cusip


classified <- read.csv("classified.2015.csv")
classified$announce_date <- as.Date(classified$announce_date, "%m/%d/%Y")
classified.2015 <- classified[(classified$announce_date< "2016-1-1"),]
c.2015 <- classified.2015[c("campaign_id","activist_objective_1","success_objective_1",
                            "Board.seats.granted.if.objective.is.not.disclosed","Board.seats.up",
                            "activist_objective_2","success_objective_2","activist_objective_3",
                            "success_objective_3")]
data = merge(c.2015, clean.shark.partial, by="campaign_id")

shark.sub.2015 <- data[which(data$activist_objective_1 != 
                                 "General undervaluation/maximize shareholder value"),]

shark.sub.2015$cusip6 <- substr(shark.sub.2015$cusip_9_digit, 1,6)
shark.sub.2015 <- shark.sub.2015[rowSums(is.na(shark.sub.2015))<ncol(shark.sub.2015),]
# Put an activists flag 

shark.sub.2015$active <- "active"

# Now, download the 13f file
setwd("~/Networks/Data/13f_test")

load("cusip_ok.short")

cusip_ok.short <- data.frame(cusip_ok.short)
cusip_ok.short <- cusip_ok.short[rowSums(is.na(cusip_ok.short))<ncol(cusip_ok.short),]

setwd("~/Networks")

shark.sub.2015 <- shark.sub.2015[rowSums(is.na(shark.sub.2015))<ncol(shark.sub.2015),]

# We are only going to need filings that concern a list of given companies as a first step
cusip.list <- unique(shark.sub.2015$cusip6)
activist.list <- unique(clean.shark.partial$cik)

same.cusips_13f <- subset( cusip_ok.short, cusip6 %in% cusip.list )
# rm(list=setdiff(ls(), c("cusip_ok.dt","shark.sub.2015")) )
same.cusips_13f$date <- as.Date(same.cusips_13f$period, "%m-%d-%Y")
# Compute the number of big investors in each cusip 

short.data <- data.frame()

for (i in 1: length(cusip.list) ){

cusip <- cusip.list[i]
subsh <- shark.sub.2015[shark.sub.2015$cusip6 == cusip, ]
subsh <- subsh[rowSums(is.na(subsh))<ncol(subsh),]

if(nrow(subsh)>0){
for( campaign in 1:(length(unique(subsh$campaign_id)))  ) {

campaign.id = unique(subsh$campaign_id)[campaign]
  
subsh.camp <- subsh[subsh$campaign_id == campaign.id, ]
subsh.camp <- subsh.camp[rowSums(is.na(subsh.camp))<ncol(subsh.camp),]

active.activist.list <-  unique(subsh.camp$cik)
passive.activist.list <- setdiff(activist.list, active.activist.list)

start <- unique(subsh.camp$announce_date)
end <- unique(subsh.camp$end_date)

# Find the quarter range in which the campaign fall in

sub <- same.cusips_13f[same.cusips_13f$cusip6 == cusip ,]
year <- sort( unique(sub$date) )
beginning.quarter <- year[ findInterval(start, year) + 1 ]
ending.quarter <- year[ findInterval(end, year) + 1 ]

if( !is.na(beginning.quarter) & !is.na(ending.quarter) ){
sub <- sub[ sub$date >= beginning.quarter & sub$date <= ending.quarter,  ]
investor.number <- length( unique(sub$cik) )
activist.set <- sub[sub$cik %in% passive.activist.list,]

active.activist.set <- sub[sub$cik %in% active.activist.list,]

if ( nrow(activist.set) >0 & nrow(active.activist.set) >0 ){

activist.set$value  = as.numeric( activist.set$value )
activist.set$total.activ.inv <- sum( as.numeric( activist.set$value ) )
activist.set$act.weight <- as.numeric( activist.set$value ) / activist.set$total.activ.inv 
total.activist.number <- length( unique(activist.set$cik) )
total.activist.size <- sum( activist.set$total.value )
activist.size.value.weighted <- sum( activist.set$act.weight*activist.set$total.value )
activist.size.average <- mean(activist.set$total.value)
active.activist.size <- sum( unique(active.activist.set$total.value) )

outcome <- unique( subsh.camp$dissident_board_seats_won )

if (  outcome %in% c(0) ){outcome.binar = 0}else{ outcome.binar = 1 }
outcome.percent <- unique( subsh.camp$dissident_board_seats_won )/unique( subsh.camp$dissident_board_seats_sought )

outcome.cl = subsh.camp$success_objective_1

if (outcome.cl %in% c("Success", "Partial")){outcome.classified=1}else{outcome.classified=0}

output <- data.frame(campaign.id, cusip, investor.number, 
                     total.activist.number, outcome.binar, outcome.percent, 
                     total.activist.size,activist.size.value.weighted,
                     outcome.classified, active.activist.size, 
                     activist.size.average,beginning.quarter,ending.quarter)
short.data <- rbind(short.data, output)

}

}
  
}
}
}

# Load compustat

setwd("~/Networks")
compustat <- read.csv("compustat_2014.csv")

compustat$cusip6 <-  substr(compustat$cusip, 1,6)
compustat.short <- compustat[c("cusip6","mkvalt", "csho", "prcc_f", "datadate")]


a = filter(compustat.short, cusip6 == "03761U" & cik == "902584")

for (i in 1:nrow(compustat.short)){
  if(!is.na(compustat.short$mkvalt[i])){
    compustat.short$market.value.mln[i] <- compustat.short$mkvalt[i]
    
  }else{compustat.short$market.value.mln[i] <- compustat.short$csho[i]*compustat.short$prcc_f[i]}
  
}

# Test on covariences and stuff
# Run simplest regresion

plot(short.data$total.activist.number,short.data$outcome.binar)

# mydata$rank <- factor(mydata$rank)
mylogit <- glm(outcome.binar ~ investor.number+log(active.activist.size), 
               data = short.data, family = "binomial")
mylogit <- glm(outcome.binar ~ total.activist.number+log(active.activist.size), 
               data = short.data, family = "binomial")
mylogit <- glm(outcome.binar ~ total.activist.number+log(activist.size.value.weighted), data = short.data, family = "binomial")
mylogit <- glm(outcome.classified ~ total.activist.number+
                 log(active.activist.size), data = short.data, family = "binomial")
mylogit <- glm(outcome.classified ~log(active.activist.size)+ log(investor.number), data = short.data, family = "binomial")

ols <- lm(outcome.classified ~ total.activist.number, data = short.data)
ols <- lm(outcome.classified ~ log(total.activist.number), data = short.data)
ols <- lm(outcome.classified ~ log(active.activist.size)+log(activist.size.value.weighted), data = short.data)

# Regressions with networks
