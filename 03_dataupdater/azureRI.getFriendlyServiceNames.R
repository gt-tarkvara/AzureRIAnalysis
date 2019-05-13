library(readxl)

if(!exists("azureRI", mode="function")) source("azureRI.R")

# TODO: add possibility to load snapshot created during specific billin period
azureRI.getFriendlyServiceNames <- function(filepath=NULL, reload=FALSE) {
  iurl <- "https://azurepricing.blob.core.windows.net/supplemental/Friendly_Service_Names.xlsx"
  
  h <- curl::new_handle()
  curl::handle_setopt(h, http_version = 0L)
  
  removeAfterLoad <- FALSE
  
  if (is.null(filepath)) {
    filepath <- paste(paste0(tempfile(), ".xlsx"), sep = .Platform$file.sep)
    removeAfterLoad <- TRUE
    reload <- TRUE
  }
  
  if (!is.null(filepath)) {
    if (file.exists(filepath) && reload) {
      file.remove(filepath)
    }
    
    if (!file.exists(filepath)) {
      req <- curl::curl_download(iurl, filepath, mode = "wb+", handle = h)
    }
  } 
  
  
  result <- tibble()
  
  if (file.exists(filepath)) {
     result <- read_excel(filepath)
  }
  result <- add_column(result, billingperiod = format(Sys.Date(), "%Y%m"))
  if (removeAfterLoad) {
    file.remove(filepath)
  }
  return(result)
}