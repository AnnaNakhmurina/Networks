# Merge the 13F data with COMPUSTAT data

compustat <- read.csv("compustat_2014.csv")
load("all.data.1300")

data_13f <- all_data.1300
# Remove "Infotable."  from names of all the entries

names(data_13f) <- gsub("^.*?infoTable.","", names(data_13f))
names(data_13f) <- gsub("^.*?shrsOrPrnAmt.","", names(data_13f))

# Create a datadate variable for consistency
data_13f$datadate <- as.integer( paste0(substring(data_13f$period, 7, 10),substring(data_13f$period, 1, 2),substring(data_13f$period, 4, 5)) )

# Clean the data_13f table -- remove white spaces etc

trim <- function (x) gsub("^\\s+|\\s+$", "", x) #this function trims leading and trailing whitespace
data_13f$cusip <- trim(data_13f$cusip)
data_13f$cusip <- trim(data_13f$cusip)
cusip_problems <- filter(data_13f, nchar(data_13f$cusip) < 9)

# Clean the data up -- remove non-stock holdings


data_13f_dt <- as.data.table(data_13f)
compustat_dt <- as.data.table(compustat)

compustat_13f.1300 <- merge(data_13f_dt , compustat_dt, by=c("datadate", "cusip")) 

# Clean it all up

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
