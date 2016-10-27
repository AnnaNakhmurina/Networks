# This file matches the names of the funds from THompson Reuters 13f database to their CIKs
# The name of the output file with matches is match_cik_13f_wrds_final
# 
# Second part of the file matches the 13f data with ciks. Output file is 13f_2000_2007_cik


rm(list=ls())
gc()
setwd("~/Networks/Analysis")

f <- read.csv("13f_2000_2007_wrds.csv")
f <- read.csv("13f_20014_wrds.csv")

# list_wrds_mgrno = f[c("mgrname", "mgrno")]
# list_wrds_mgrno= unique(list_wrds_mgrno)

# save(list_wrds_mgrno, file="list_wrds_mgrno_2")
# 
# load("list_wrds_mgrno_1")
# list_mgrno = list_wrds_mgrno
# 
# load("list_wrds_mgrno_2")
# 
# list_mgrno=rbind(list_mgrno, list_wrds_mgrno)
# list_mgrno = unique(list_mgrno)

# save(list_mgrno, file="list_mgrno")

# list_13_funds_wrds=c(as.vector(list_07),as.vector(list_13))
# save(list_13_funds_wrds, file="list_13_funds_wrds")


load("list_13_funds_wrds")
wrds_upper <-  toupper(  gsub(",|\\.|'|\\?|/|-", "", list_13_funds_wrds) )

load("file.list.13fg")
fund.data <- unique( file.list.13fg[,c(1,3)] )
fund.data$company_name.long <-  toupper(  gsub(",|\\.|'|\\?|/|-", "", fund.data$company_name) )

# Do an exact match

matched.funds.exact <- data.frame()

for(i in 1:length(wrds_upper)){
  
  to.find <- wrds_upper[i]
  
  row.number <- pmatch(to.find, fund.data$company_name.long)
  
  if ( ! is.na(row.number) ){
    # print(i)
    out <- cbind( i, fund.data[row.number,], to.find)
    matched.funds.exact <- rbind(matched.funds.exact,out)
  }
  
}

# Get the non-matched list

is <- matched.funds.exact$i
all <- c(  1:length(wrds_upper) )
nis <- setdiff(all,is)

matched.funds.exact_13f_wrds = matched.funds.exact
matched.funds.exact_13f_wrds = matched.funds.exact[c("to.find", "cik")]
names(matched.funds.exact_13f_wrds) = c("fund_name_wrds", "cik")
save(matched.funds.exact_13f_wrds, file="matched.funds.exact_13f_wrds")

not_matched_13f_wrds <- wrds_upper[nis]

# Now substitute the names of the not_matched_13f_wrds for the longer names from Katya Volkova's table


subst = read.csv("TR_SEC_substitutions.csv")
subst$original = toupper(subst$original)
subst$subst = toupper(subst$subst)


not_matched_substituted = not_matched_13f_wrds
for (i in 1:nrow(subst) ){
  not_matched_substituted = gsub(paste0(" ",subst$original[i]," "), paste0(" ", subst$subst[i], " "), not_matched_substituted)
}

# Note that not all the funds names were properly substituted by this exercise!!!! only the words with whitespeces surrounding them
# Merge two vectors for future reference

not_matched_df = as.data.frame( cbind(not_matched_13f_wrds,not_matched_substituted))

# Do an exact match for the substituted vector

subst.funds.exact <- data.frame()

for(i in 1:nrow(not_matched_df)){
  
  to.find <- not_matched_df$not_matched_substituted[i]
  
  row.number <- pmatch(to.find, fund.data$company_name.long)
  
  if ( ! is.na(row.number) ){
    # print(i)
    out <- cbind( i, fund.data[row.number,], not_matched_df[i,])
    subst.funds.exact <- rbind(subst.funds.exact,out)
  }
  
}

subst.funds.exact_13f_wrds = subst.funds.exact[c("not_matched_13f_wrds","cik")]
names(subst.funds.exact_13f_wrds) = c("fund_name_wrds", "cik")

# Merge matched funds to the already existent matched fund database
matched.funds.exact_13f_wrds = rbind(matched.funds.exact_13f_wrds, subst.funds.exact_13f_wrds)
save(matched.funds.exact_13f_wrds, file="matched.funds.exact_13f_wrds")

# Now delete already matched funds from the list

is <- c(matched.funds.exact$i,subst.funds.exact$i)
all <- c(  1:length(wrds_upper) )
nis <- setdiff(all,is)

not_matched_13f_wrds_stage2 <- wrds_upper[nis]

simplify_names <- function(names) {
  names <- toupper(  gsub(",|\\.|'|\\?|/|-", "", names) )
  names <- gsub(toupper(paste0("Inc|LLC|Corp|Corporation|Ltd|Asset|Management|Partners|Investments|",
                               "Investors|Investment|Investors|Holdings|Limited|LC|Associates|International|",
                               "LP|Advisors|Advisers|Advisory|Group|Solutions|Co|Technologies|l l c|mgmt|co|invt|advisors|ltd|lp|corp|
                               ptnr|partners|advr|advs|l p|\\(|\\)")
  ), "", names)
  names <- gsub("\\s+", " ", names) # remove multiple spaces
  names <- gsub(" AND ", " & ", names)
  names <- gsub(" ;", ";", names)   # remove spaces before ';'
  names <- gsub("^ | $", "", names) # remove leading and trailing spaces
  return(names)
}

stage2_simplified = simplify_names(not_matched_13f_wrds_stage2)
stage2 = data.frame(stage2_simplified, not_matched_13f_wrds_stage2)
fund.data$company_name.long.simpl <- simplify_names(fund.data$company_name.long)


stage2.exact <- data.frame()

for(i in 1:nrow(stage2)){
  
  to.find <- stage2$stage2_simplified[i]
  
  row.number <- pmatch(to.find, fund.data$company_name.long.simpl)
  
  if ( ! is.na(row.number) ){
    # print(i)
    out <- cbind( i, fund.data[row.number,], stage2[i,])
    stage2.exact <- rbind(stage2.exact,out)
  }
  
}

# Write this dataframe into the csv file and check the rows for the errors

write.csv(stage2.exact, file="stage2.exact.csv")

stage2_checked = read.csv("stage2.exact_checked.csv")
stage2_checked= stage2_checked[c("not_matched_13f_wrds_stage2", "cik")]
names(stage2_checked) = c("fund_name_wrds", "cik")
load("matched.funds.exact_13f_wrds")

matched.funds.exact_13f_wrds = rbind(matched.funds.exact_13f_wrds,stage2_checked)

save(matched.funds.exact_13f_wrds, file="matched.funds.exact_13f_wrds")

a=as.character(matched.funds.exact_13f_wrds$fund_name_wrds)
b=as.character(stage2.exact$not_matched_13f_wrds_stage2)
matched.vector = unique(c(a,b))

not_matched_13f_wrds_stage3 <- setdiff(wrds_upper, matched.vector)

stage3_simplified = simplify_names(not_matched_13f_wrds_stage3)
stage3 = data.frame(stage3_simplified, not_matched_13f_wrds_stage3)
fund.data$company_name.long.simpl <- simplify_names(fund.data$company_name.long)


# Do an approximate match 
require(stringdist)

stage3.exact <- data.frame()

for(i in 1:nrow(stage3)){
  
  to.find <- stage3$stage3_simplified[i]
  
  row.number <- pmatch(to.find, fund.data$company_name.long.simpl)
  
  if ( ! is.na(row.number) ){
    # print(i)
    out <- cbind( i, fund.data[row.number,], stage3[i,])
    stage3.exact <- rbind(stage3.exact,out)
  }
  
}

stage3.approx <- data.frame()

for ( j in 1:length(not_matched_13f_wrds_stage3) ) {
  print(j)
  name <- not_matched_13f_wrds_stage3[j]
  
  name_dist <- stringdist(name, fund.data$company_name.long, 
                          method = "lv", 
                          weight = c(d = 1, i = 1, s = 1, t = 1), 
                          maxDist = Inf)
  min_name_dist <- min(name_dist)
  min_name_dist_ids <- which(name_dist==min_name_dist)
  name_match <- fund.data$company_name.long[min_name_dist_ids]
  matched_1 <- data.frame( j,
                           name_to_find=name,
                           names_matched=name_match,
                           names_matched_full=fund.data$company_name.long[min_name_dist_ids],
                           cik=fund.data$cik[min_name_dist_ids],
                           names_dist=min_name_dist,
                           names_dist_normed=min_name_dist/nchar(name),
                           stringsAsFactors = FALSE )
  stage3.approx <- rbind(stage3.approx,matched_1)
}

# Check the matches:

write.csv(stage3.approx, file="stage3.approx.csv")

checked_stage3 = read.csv("stage3.approx_checked.csv")
checked_stage3 = unique(checked_stage3)
names(checked_stage3) = names(stage3.approx)
checked_stage3 = checked_stage3[which(!is.na(checked_stage3$j)),]
matched_stage3 = checked_stage3[c("name_to_find","cik")]
names(matched_stage3) = names(matched.funds.exact_13f_wrds)

matched.funds.exact_13f_wrds = rbind(matched.funds.exact_13f_wrds, matched_stage3)
save(matched.funds.exact_13f_wrds, file="matched.funds.exact_13f_wrds")

# Final stage - manual check 


a=as.character(matched.funds.exact_13f_wrds$fund_name_wrds)
b=as.character(stage2.exact$not_matched_13f_wrds_stage2)
matched.vector = unique(c(a,b))

not_matched_13f_wrds_stage4 <- setdiff(wrds_upper, matched.vector)

write.csv(not_matched_13f_wrds_stage4, file="stage4.csv")

# Match potentially same funds (funds with the same mgrno) 

load("list_mgrno")
list_mgrno$mgrname <-  toupper(  gsub(",|\\.|'|\\?|/|-", "", list_mgrno$mgrname) )
load("matched.funds.exact_13f_wrds")


test =merge(list_mgrno, matched.funds.exact_13f_wrds, by.x="mgrname", by.y="fund_name_wrds", all.x = TRUE)
test =unique(test)

to_match_4= read.csv("stage4.csv")
t4 = merge(list_mgrno, to_match_4, by.x="mgrname", by.y="name")
t4 = unique(t4)

mgrno_list = unique(t4$mgrno)

mgrno_suspects = test[which(test$mgrno %in% mgrno_list),]
mgrno_suspects = unique(mgrno_suspects)

# not_there = intersect(mgrno_list, mgrno_suspects$mgrno)

merged = merge(t4, mgrno_suspects, by="mgrno")
length((merged$cik.y[is.na(merged$cik.y)]))
write.csv(merged, file="stage4_refined.csv")

# Load them!
load("matched.funds.exact_13f_wrds")
load("list_mgrno")
list_mgrno$mgrname <-  toupper(  gsub(",|\\.|'|\\?|/|-", "", list_mgrno$mgrname) )

matched_mgrno = merge(matched.funds.exact_13f_wrds, list_mgrno, by.x="fund_name_wrds", by.y="mgrname")
matched_mgrno = unique(matched_mgrno)

stage4_refined_checked = read.csv("stage4_refined_checked.csv")
s4 = stage4_refined_checked[c("mgrname.x", "cik","mgrno")]
names(s4)= names(matched_mgrno)

matched_mgrno = rbind(matched_mgrno, s4)
match_cik_13f_wrds_final = unique(matched_mgrno)

save(match_cik_13f_wrds_final, file="match_cik_13f_wrds_final")


#--------------------------------This part cleans the data itself-------------------------

# Match the 13f data with ciks. Output file is 13f_2000_2007_cik
rm(list=ls())
gc()
setwd("~/Networks/Analysis")

load("match_cik_13f_wrds_final")
names(match_cik_13f_wrds_final) =c("mgrname", "cik","mgrno")

# f <- read.csv("13f_2000_2007_wrds.csv")
# f <- read.csv("13f_2008_2013_wrds.csv")
f <- read.csv("13f_20014_wrds.csv")
f = merge(match_cik_13f_wrds_final, f, by="mgrname")
f$date = as.Date( as.character( f$rdate  ) , "%Y%m%d"  )
# fd$cusip6 <- substr(fd$cusip, 1,6)
# cik_13f  = f

# load("13f_2000_2007_cik")
# Add correct cusips

a =(f$cusip)
old_cusip = unique(a) 
b = lapply(old_cusip, sub, pattern='^0+([1-9])', replacement='\\1')
correct_cusip = unlist(b)

correct_cusips = data.frame(old_cusip, correct_cusip)

cik_13f = merge(f, correct_cusips, by.x ="cusip", by.y="old_cusip")
cik_13f$cusip6 <- substr(cik_13f$correct_cusip, 1,6)


save(cik_13f, file="13f_2014_cik")
load("13f_2008_2013_cik")
fd = rbind(cik_13f, fd)
load("13f_2000_2007_cik")
cik_13f = rbind(cik_13f, fd)

save(cik_13f, file="13f_2000_2014_cik")

# ---- Finally, aggregate all data from  2000 to 2014
library(data.table)
# load("13f_2000_2007_cik")
# load("13f_2008_2013_cik")
load("13f_2014_cik")
# Value
cik_13f$value  = cik_13f$prc*cik_13f$shares/(1000000)
cik_13f = cik_13f[which(!is.na(cik_13f$value)),]
cik_13f = as.data.table(cik_13f)
cik_13f <-cik_13f[, total.value := sum( as.numeric(value) ), by = list(cik, rdate)]
# cik_13f = as.data.frame(cik_13f)

save(cik_13f, file="13f_2000_2007_cik")

# save(cik_13f, file="13f_2000_2014_cik")


#----------------- Add the set of variables needed for the network formation

rm(list=ls())
gc()
setwd("~/Networks/Analysis")

# load("13f_2000_2014_cik")
library(data.table)
# Sum over all cusip6 investments to get rid of ambuguity

names= c("date","cusip6","total.value" )

dt = as.data.table(cik_13f)
# dt = dt[which(dt$date < "2007-12-31")]
dt$sole <- as.numeric(dt$sole)
dt$shared <- as.numeric(dt$shared)
dt$no <- as.numeric(dt$no)
# dt$shrout1 <- as.numeric(dt$shrout1)
dt_14= dt[,  list(value.mln.all = sum(value),share.amt =sum(shrout1),sole =sum(sole), 
                                  shared=sum(shared), no=sum(no) ), 
                                  by=c(names, "cik")]
# save(dt_14, file="dt_14")
# save(dt_07, file="dt_07")
save(dt_13, file="dt_13")

load("dt_07")
load("dt_13")
load("dt_14")

cik_13f = rbind(dt_07, dt_13)
cik_13f = rbind(cik_13f, dt_14)

save(cik_13f, file="13f_2000_2014_cik_dt")

