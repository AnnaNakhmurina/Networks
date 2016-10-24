  setInternet2(use=FALSE)
library(XML)
library(xml2)
library(RCurl)
library(dplyr)
library(stringr)
library(data.table)
library(gtools)

  
setwd("~/Networks/Analysis")
  
load("file.list.13f")
list_raw <- filter(file.list.13f, date_filed < "2013-12-31")
list_raw <- filter(list_raw, date_filed > "2003-12-31")
head_list <- head(list_raw)
list <- list_raw

setwd("~/Networks/Analysis/Data/13f_test")

list$file_name <- as.character(list$file_name) #turn the path into a character from a factor
list$form_type <- gsub(",","",list$form_type) #get rid of commas in names so that we can save as csv without quotes
#Path variable will have the path to the info table .xml file
#The path to the .xml file is the same as to the .txt file except it does not have dashes and the address is
#sec.gov/Archives instead of "ftp: sec"
list$path <- substr(list$file_name,1,nchar(list$file_name)-4) #create a new variable path
list$path <- gsub("-","",list$path)
list$path <- paste("https://www.sec.gov/Archives/", list$path, sep = "")
list$file_name <- paste("ftp://ftp.sec.gov/",list$file_name, sep = "") 
#It looks like we  need to prepopulate some of the characteristics that we will be reading from the .txt file
list$period <-0


for (i in 34943:nrow(list)) {
  txt <- readLines(list$file_name[i]) #reads in the .txt file (We only need the first 1000 lines since the name of the xml file will be there)
  l <- str_locate(txt,"<FILENAME>") #finds where the <FILENAME> tag is 
  start <- max(which(l == 1)) #there are two <FILENAME> tags, I need the second one because that one refers to the info table xml
  filename <- substr(noquote(txt[start]),11,10000) #extract the filename from that one line in .txt file skipping the <FILENAME> tag
  list$path[i] <- paste(list$path[i],"/",filename, sep = "") #create a path to the info table .xml
  #the code below grabs all kinds of data from the header, it strips the tags
  #get reporting period
  l <- str_locate(txt,"<periodOfReport>")
  start <- min(which(l != "NA"))
  value <- substr(noquote(txt[start]),0,100)
  value <- substr(value,str_locate(value,">")[1]+1,str_locate(value,"</")[1]-1)
  list$period[i] <- value
  if (i/10 == floor(i/10)) {print(i)}  #print every 10th value to monitor progress of the loop
}

# VADIM, START HERE:

save(list, file="list.2015")


setwd("~/Networks/Data/13f_test")
load("list.2015")
list$path <- as.character(list$path)


### 
# all <- xmlToDataFrame(xmlParse(read_xml("example.xml")))
all_data <- data.frame()
for (i in  c(33244:length(list$path)) ) {
print(i)
# a <- read_xml(list$path[i]) 
d <- download.file(list$path[i], destfile=paste0("entry",i, ".xml"))
data <- xmlParse(paste0("entry",i, ".xml"))
xml_data <- xmlToList(data)
xml_data <- Filter(Negate(function(x) is.null(unlist(x))), xml_data)

xml_list <- lapply(1:length(xml_data), function(x)  as.data.table( t(unlist(xml_data[x])) )  )

# for (x in 1:length(xml_data)){
#     
#   y=as.data.table( t(unlist(xml_data[x])) )
# }

xml_table_converted <- do.call(smartbind, xml_list)
xml_table_converted$cik <- list$cik[i]
xml_table_converted$fund_name <- list$company_name[i]
xml_table_converted$period <- list$period[i]
all_data <- bind_rows(all_data,xml_table_converted)
}

all_data.33243.33478 <- all_data
# all_data.10662 <- bind_rows(all_data.8102, all_data)
save(all_data.33243.33478, file="all_data.33243.33478")

# The part to download 13f before 2013

# setInternet2(use=FALSE)
library(XML)
library(xml2)
library(RCurl)
library(dplyr)
library(stringr)
library(data.table)
library(gtools)

load("file.list.13f")
list_raw <- filter(file.list.13f, date_filed > "2013-12-31" & date_filed <= "2014-12-31")
head_list <- head(list_raw)
list <- list_raw

list$file_name <- as.character(list$file_name) #turn the path into a character from a factor
list$form_type <- gsub(",","",list$form_type) #get rid of commas in names so that we can save as csv without quotes
list$path <- substr(list$file_name,1,nchar(list$file_name)-4) #create a new variable path
list$path <- gsub("-","",list$path)
list$path <- paste("https://www.sec.gov/Archives/", list$path, sep = "")
list$file_name <- paste("ftp://ftp.sec.gov/",list$file_name, sep = "") 
#It looks like we  need to prepopulate some of the characteristics that we will be reading from the .txt file
list$period <-0

i_start <- 10991
i_end <- nrow(list)
max_trials <- 20

print(paste('Total:',nrow(list)))

all_data <- data.frame()

write(paste( 'start', Sys.time(), sep='\t\t' ), file="check_point.log", append=TRUE)

add_next_data <- function(i,list) {
  print(paste('Downloading',list$file_name[i]))
  system( paste("wget --no-check-certificate -O tmp",list$file_name[i]) )
  txt <- readLines('tmp', n=1000) #reads in the .txt file (We only need the first 1000 lines since the name of the xml file will be there)
  print('done')
  l <- str_locate(txt,"<CAPTION>") #finds where the <FILENAME> tag is 
  start <- min(which(l == 1)) #there are two <FILENAME> tags, I need the second one because that one refers to the info table xml
  filename <- substr(noquote(txt[start]),11,10000) #extract the filename from that one line in .txt file skipping the <FILENAME> tag
  full_path <- paste(list$path[i],"/",filename, sep = "") #create a path to the info table .xml
  #the code below grabs all kinds of data from the header, it strips the tags
  #get reporting period
  l <- str_locate(txt,"<periodOfReport>")
  start <- min(which(l != "NA"))
  value <- substr(noquote(txt[start]),0,100)
  value <- substr(value,str_locate(value,">")[1]+1,str_locate(value,"</")[1]-1)
  list$period[i] <- value
  # if (i/10 == floor(i/10)) {print(i)}  #print every 10th value to monitor progress of the loop
  
  print(paste('Downloading',full_path))
  #d <- download.file(full_path, destfile=paste0("tmp"), method='wget')
  system( paste("wget --no-check-certificate -O tmp",full_path) )
  print('done')
  data <- xmlParse(paste0("tmp"))
  xml_data <- xmlToList(data)
  xml_data <- Filter(Negate(function(x) is.null(unlist(x))), xml_data)
  
  xml_list <- lapply(1:length(xml_data), function(x)  as.data.table( t(unlist(xml_data[x])) )  )
  
  print('done parsing xml')
  # for (x in 1:length(xml_data)){
  #   y=as.data.table( t(unlist(xml_data[x])) )
  # }
  
  xml_table_converted <- do.call(smartbind, xml_list)
  xml_table_converted$cik <- list$cik[i]
  xml_table_converted$fund_name <- list$company_name[i]
  xml_table_converted$period <- list$period[i]
  return(xml_table_converted)
}

for (i in i_start:i_end) {
  print(i)
  
  trials <- 1
  repeat {
    success <- TRUE
    xml_table_converted <- tryCatch(add_next_data(i,list), 
                                    error = function(e) {
                                      print(paste("Failed after",trials,"trials with message:",e))
                                      trials <<- trials+1
                                      success <<- FALSE
                                    })
    if ( success | trials > max_trials ) break
  } 
  
  if ( success ) {
    print(paste("Success after",trials,"trials"))
    all_data <- bind_rows(all_data,xml_table_converted)
  } else {
    write(paste( i, Sys.time(), sep='\t\t' ), file="errors.log", append=TRUE)
  }
  
  if ( i%%100 == 0 ) {
    print("Saving checkpoint")
    print(i)
    write(paste( i, Sys.time(), sep='\t\t' ),  file="check_point.log", append=TRUE)
    save(all_data, file="check_point")
  }
  
}

# VADIM, START HERE:

# save(list, file="list.2015")
# 
# 
# setwd("~/Networks/Data/13f_test")
# load("list.2015")
# list$path <- as.character(list$path)


### 
# all <- xmlToDataFrame(xmlParse(read_xml("example.xml")))

# for (i in  c(22069:25000) ) {
# print(i)
# a <- read_xml(list$path[i]) 


all_data.20309.21934 <- all_data
# all_data.10662 <- bind_rows(all_data.8102, all_data)
save(all_data.20309.21934, file="all_data.20309.21934")
close(LOG)

