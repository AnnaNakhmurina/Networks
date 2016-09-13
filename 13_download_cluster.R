require(Rcurl)

load("file.list")

Sys.setenv(EDGAR_DIR="/home/anakhmur")
raw_directory <- Sys.getenv("EDGAR_DIR")

get_text_file <- function(path) {
  
  local_filename <- file.path(raw_directory, path)
  # Only download the file if we don't already have a local copy
  download.text <- function(path) {
    
    ftp <- file.path("http://www.sec.gov/Archives", path) 
    cat(dirname(local_filename), "\n")
    dir.create(dirname(local_filename), showWarnings = FALSE)
    if (!file.exists(local_filename)) {
      try(download.file(url=ftp, destfile=local_filename))
    }
  }                      
  
  #     print(path[!file.exists(local_filename) & !is.na(path)])
  lapply(path[!file.exists(local_filename) & !is.na(path)],
         download.text)    
  
  # Return the local filename if the file exists
  return(file.exists(local_filename))
}

file.list$have_file <- NA
to.get <- 1:length(file.list$have_file) 

system.time({
  file.list$have_file[to.get] <- 
    unlist(mclapply(file.list$file_name[to.get], get_text_file,
                    mc.preschedule=FALSE, mc.cores=1))
})
