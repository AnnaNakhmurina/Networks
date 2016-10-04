rm(list=ls())
gc()

require(stringdist)

setwd("~/Documents/Ann/match_names")

simplify_names <- function(names) {
  names <- toupper(  gsub(",|\\.|'|\\?|/|-", "", names) )
  names <- gsub(toupper(paste0("Inc|LLC|Corp|Corporation|Ltd|Asset|Management|Partners|Investments|",
                               "Investors|Investment|Investors|Holdings|Limited|LC|Associates|International|",
                               "LP|Advisors|Advisers|Advisory|Group|Solutions|Co|Technologies|\\(|\\)")
                        ), "", names)
  names <- gsub("\\s+", " ", names) # remove multiple spaces
  names <- gsub(" AND ", " & ", names)
  names <- gsub(" ;", ";", names)   # remove spaces before ';'
  names <- gsub("^ | $", "", names) # remove leading and trailing spaces
  return(names)
}

# names to match:
not_matched <- read.csv("not.matched.long.csv")
not_matched <- not_matched[ complete.cases(not_matched), ]
not_matched <- unique(not_matched)
not_matched <- not_matched[ order(not_matched$list.shark), ]
not_matched$names_tomatch <- simplify_names(not_matched$long)

# names with cik
load("fund.data")
fund_data <- unique(fund.data[c("company_name","cik")])
rm(fund.data)
fund_data <- fund_data[ order(fund_data$company_name), ]
fund_data$company_name_clean <- simplify_names(fund_data$company_name)


shark_matched <- data.frame()

for ( i in 1:nrow(not_matched) ) {
  if ( !(i%%100) ) { print(paste(i,"/",nrow(not_matched))) }
  names_split_full <- strsplit(as.character(not_matched$long[i]),";")[[1]]
  names_split <- strsplit(as.character(not_matched$names_tomatch[i]),";")[[1]]
  
  names_match <- list()
  names_dist <- list()
  
  matched_i <- data.frame()
  
  for ( j in 1:length(names_split) ) {
    name <- names_split[j]
    name_dist <- stringdist(name, fund_data$company_name_clean, 
                            method = "lv", 
                            weight = c(d = 1, i = 1, s = 1, t = 1), 
                            maxDist = Inf)
    min_name_dist <- min(name_dist)
    min_name_dist_ids <- which(name_dist==min_name_dist)
    name_match <- fund_data$company_name_clean[min_name_dist_ids]
    matched_1 <- data.frame( id=i,
                             names_tomatch=not_matched$list.shark[i],
                             names_split_full=names_split_full[j],
                             names_split=name,
                             names_match=name_match,
                             names_match_full=fund_data$company_name[min_name_dist_ids],
                             cik=fund_data$cik[min_name_dist_ids],
                             names_dist=min_name_dist,
                             names_dist_normed=min_name_dist/nchar(name),
                             stringsAsFactors = FALSE )
    matched_i <- rbind(matched_i,matched_1)
  }
  
  shark_matched <- rbind(shark_matched,matched_i)
}

write.csv(shark_matched, file="shark_matched.csv",row.names = FALSE)

shark_matched_cut <- shark_matched[ shark_matched$names_dist_normed < 0.4 , ]
write.csv(shark_matched_cut, file="shark_matched_cut.csv",row.names = FALSE)
