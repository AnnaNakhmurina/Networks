# Create functions to get metadata for SEC filings ----

library(curl)

getSECIndexFile <- function(year, quarter) {
  
  library(curl)
  # Download the zipped index file from the SEC website
  tf <- tempfile()
  result <- try(curl_download(
    url=paste("http://www.sec.gov/Archives/edgar/full-index/",
              year,"/QTR", quarter, "/company.zip",sep=""),
    destfile=tf))
  
  # If we didn't encounter and error downloading the file, parse it
  # and return as a R data frame
  if (!inherits(result, "try-error")) {
    
    # Small function to remove leading and trailing spaces
    trim <- function (string) {
      Encoding(string) <- "latin1"
      string <- enc2native(string)
      gsub("^\\s*(.*?)\\s*$","\\1", string, perl=TRUE)
    }
    
    # Read the downloaded file
    raw.data <- readLines(con=(zz<- unz(description=tf,
                                        filename="company.idx")))
    close(zz)
    raw.data <- raw.data[11:length(raw.data)] # Remove the first 10 rows.
    
    # Parse the downloaded file and return the extracted data as a data frame
    company_name <- trim(substr(raw.data,1,62))
    form_type <- trim(substr(raw.data,63,74))
    cik <- trim(substr(raw.data,75,86))
    date_filed <- as.Date(substr(raw.data,87,98))
    file_name <- trim(substr(raw.data,99,150))
    rm(raw.data)
    return(data.frame(company_name, form_type, cik, date_filed, file_name))
  } else { return(NULL)}
}

addIndexFileToDatabase <- function(data) {
  if (is.null(data)) return(NULL)
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  
  password <- scan(".pgpass.txt", what="")
  
  pg <- dbConnect(drv, dbname = "filings",
                  host = "localhost", port = 5432,
                  user = "anakhmur", password = password)
  
  rs <- dbGetQuery(pg, "CREATE SCHEMA IF NOT EXISTS filings")
  rs <- dbWriteTable(pg, c("filings", "filings"), data,
                     append=TRUE, row.names=FALSE)
  dbDisconnect(pg)
  return(rs)
}

require("RPostgreSQL")
drv <- dbDriver("PostgreSQL")

password <- scan(".pgpass.txt", what="")

pg <- dbConnect(drv, dbname = "filings",
                host = "localhost", port = 5432,
                user = "anakhmur", password = password)

# dbGetQuery(pg, "DROP TABLE IF EXISTS filings.filings")

for (year in 1993:2015) {
  for (quarter in 1:4) {
    if(dbExistsTable(pg, c("filings", "filings"))) {
      dbGetQuery(pg, paste(
        "DELETE
        FROM filings.filings
        WHERE extract(quarter FROM date_filed)=", quarter,
        " AND extract(year FROM date_filed)=", year))
    }
    addIndexFileToDatabase(getSECIndexFile(year, quarter))
  }
}

#Add data for 2016
for (year in 2016) {
  for (quarter in 1:2) {
    if(dbExistsTable(pg, c("filings", "filings"))) {
      dbGetQuery(pg, paste(
        "DELETE
        FROM filings.filings
        WHERE extract(quarter FROM date_filed)=", quarter,
        " AND extract(year FROM date_filed)=", year))
    }
    addIndexFileToDatabase(getSECIndexFile(year, quarter))
  }
  }
rs <- dbDisconnect(pg)

#----This part downloads 13Ds-----------------------------

# Refine the code to only select 13-type filings
sample <- getSECIndexFile(2012,1)
names<- unique(sample$form_type)
# select only the files that have 13 in the pattern
filings_13d <- sample[ which(sample$form_type %in% grep("13D", names, value=TRUE)),]

# Define the functions

extract_value <- function(text, tag) {
  tag <- paste("^<", tag, ">", sep="")
  gsub(tag, "", text[grep(tag, text)])
}

find_text <- function(tag, text) {
  temp <- grep(tag, text)
  if (length(temp)) return(min(temp))
  else (return(0))
}

extract_portion <- function(the_text=NULL, tag) {
  # if (is.null(the_text) | is.null) return(NULL)
  
  beg.tag <- paste("^<", tag, ">", sep="")
  end.tag <- paste("^</", tag, ">", sep="")
  
  beg <-  find_text(beg.tag, the_text)
  end <-  find_text(end.tag, the_text)
  
  if(end>beg) { the_text[beg:end] } else {
    stop()
  }
}

parse13d <- function(text) {
  ## Extract relevant data from a 13D filing.
  ## Also works with 13G filings.
  sub.txt <- extract_portion(text, "SUBJECT-COMPANY")
  sub_cik <- extract_value(sub.txt, "CIK")
  sub_name <- extract_value(sub.txt, "CONFORMED-NAME")
  
  filer.txt <- try(extract_portion(text, "FILED-BY"))
  if (class(filer.txt) != "try-error") {
    filer_cik <- extract_value(filer.txt, "CIK")
    filer_name <- extract_value(filer.txt, "CONFORMED-NAME")
    return(data.frame(sub_cik, sub_name, filer_cik, filer_name))
  } else {
    return(data.frame(sub_cik=NA, sub_name=NA, filer_cik=NA, filer_name=NA))
  }
}

getSGMLlocation <- function(path) {
  ## Convert a file_name from filings.filings to a path to
  ## the associated SGML file
  sgml_basename <- basename(gsub(".txt$", ".hdr.sgml", path, perl=TRUE))
  sgml_path <- file.path(dirname(path),
                         gsub("(-|\\.hdr\\.sgml$)", "",
                              sgml_basename, perl=TRUE))
  
  ftp <- file.path("http://www.sec.gov/Archives", sgml_path, sgml_basename)
  return(ftp)
}

extract13Ddata <- function(file_name) {
  text <- try(readLines(getSGMLlocation(file_name)), TRUE)
  if (class(text) == "try-error") return(NA)
  if (length(text)==0) return(NA)
  return(data.frame(file_name, parse13d(text)))
}



require("RPostgreSQL")
drv <- dbDriver("PostgreSQL")

password <- scan(".pgpass.txt", what="")

pg <- dbConnect(drv, dbname = "filings",
                host = "localhost", port = 5432,
                user = "anakhmur", password = password)

filings <- dbGetQuery(pg, "
                      SET work_mem='1GB';
                      SELECT *
                      FROM filings.filings
                      WHERE file_name NOT IN (
                      SELECT file_name
                      FROM filings.filing_details_13d)")
dim(filings)

removeErrors <- function(a_list) {
  a_list[unlist(lapply(a_list, function(x) { class(x)!="try-error"}))]
}


library(parallel)
batch_rows <- 100
for (i in 0:floor((dim(filings)[1])/batch_rows)) {
  print(paste0("Currently calculating ", i, " out of ", 
               floor((dim(filings)[1])/batch_rows), ", i.e. ",
               round(100*i/floor((dim(filings)[1])/batch_rows),3), " %."))
  filing_list <- NULL
  from <- i*batch_rows+1
  to <- min((i+1)*batch_rows, dim(filings)[1])
  
  if (to >= from) {
    range <- from:to
  } else {
    range <- NULL
  }
  
  filing_list <- mclapply(filings$file_name[range], extract13Ddata,
                          mc.cores=1)
  
  if (!is.null(filing_list) & !is.null(range)) {
    filing_list <- removeErrors(filing_list)
    filing_details <- do.call(rbind, filing_list)
    rs <- dbWriteTable(pg, c("filings","filing_details_13d"), filing_details,
                       append=TRUE, row.names=FALSE)
    
    rs <- dbGetQuery(pg, "VACUUM filings.filing_details_13d")
  }
}

rs <- dbGetQuery(pg, "
                 DELETE FROM filings.filing_details_13d
                 WHERE file_name IS NULL;")

rs <- dbDisconnect(pg)

library(RPostgreSQL)

# Get filings
drv <- dbDriver("PostgreSQL")
password <- scan(".pgpass.txt", what="")

pg <- dbConnect(drv, dbname = "filings",
                host = "localhost", port = 5432,
                user = "anakhmur", password = password)



file.list <- dbGetQuery(pg, "
                        SET work_mem='1GB';
                        SELECT * 
                        FROM filings.filings
                        WHERE form_type IN ('SC 13G', 'SC 13G/A', 'SC 13D', 'SC 13D/A')
                        AND file_name NOT IN (
                        SELECT file_name 
                        FROM filings.cusip_cik)
                        ")

file.list.13fg <- dbGetQuery(pg, "
                        SET work_mem='1GB';
                        SELECT * 
                        FROM filings.filings
                        WHERE form_type IN ('13F-NT', '13F-HR/A', '13F-NT/A','13F-E','13F-HR', '13F-E/A','SC 13G', 'SC 13G/A', 'SC 13D', 'SC 13D/A')
                        AND file_name NOT IN (
                        SELECT file_name 
                        FROM filings.cusip_cik)
                        ")
rs <- dbDisconnect(pg)

source("C:/Users/anakhmur/Documents/Networks/download_filing_functions.R")

# Now, pull text files for each filing ----
file.list$have_file <- NA
to.get <- 1:length(file.list$have_file) #

#First 10th
to.get <- 1:110000
  
  # to.get <- 1:123379

library(parallel)


system.time({
  file.list$have_file[to.get] <- 
    unlist(mclapply(file.list$file_name[to.get], get_text_file,
                    mc.preschedule=FALSE, mc.cores=1))
})
