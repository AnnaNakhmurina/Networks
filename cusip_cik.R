
library("RPostgreSQL")


password <- scan(".pgpass.txt", what="")
pg <- dbConnect(drv, dbname = "filings",
                host = "localhost", port = 5432,
                user = "anakhmur", password = password)


if (!dbExistsTable(pg, c("filings", "cusip_cik"))) {
  dbGetQuery(pg, "
             CREATE TABLE filings.cusip_cik
             (
             file_name text,
             cusip text,
             cik integer,
             company_name text,
             format text
             )
             GRANT SELECT ON TABLE filings.cusip_cik TO crsp_basic;
             CREATE INDEX ON filings.cusip_cik (cusip);
             CREATE INDEX ON filings.cusip_cik (cik);")
}

# Note that this assumes that streetevents.calls is up to date.
file_list <- dbGetQuery(pg, "
                        SET work_mem='1GB';
                        SELECT file_name
                        FROM filings.filings 
                        WHERE form_type IN ('SC 13G', 'SC 13G/A', 'SC 13D', 'SC 13D/A')
                        EXCEPT 
                        SELECT file_name 
                        FROM filings.cusip_cik
                        ORDER BY file_name")

rs <- dbDisconnect(pg)

# Create function to parse a SC 13D or SC 13F filing ----
parseFile <- function(file_name) {
  
  # Parse the indicated file using a Perl script
  system(paste("extract_cusips.pl", file_name),
         intern = TRUE)
}

# Apply parsing function to files ----
library(parallel)
system.time({
  res <- unlist(mclapply(file.list$file_name[1], parseFile, mc.cores=1))
})