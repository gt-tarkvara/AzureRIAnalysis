library(readxl)

if(!exists("azureRI", mode="function")) source("azureRI.R")

# TODO: add possibility to load snapshot created during specific billin period
#azureRI.getFriendlyServiceNames <- function(filepath=NULL, reload=FALSE) {
azureRI.getFriendlyServiceNames <- function(apiObj, billingPeriod) {
  
  # Source url of information
  iurl <- "https://azurepricing.blob.core.windows.net/supplemental/Friendly_Service_Names.xlsx"
  
  h <- curl::new_handle()
  curl::handle_setopt(h, http_version = 0L)
  
  removeAfterLoad <- FALSE
  
  temp_dir <- apiObj$cachedir
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = T)
  }
  
  filepath <- paste(temp_dir, paste0("friendlyservicenames-", billingPeriod, ".xlsx"), sep = .Platform$file.sep)
  
  
  if (!file.exists(filepath)) {
    req <- curl::curl_download(iurl, filepath, mode = "wb+", handle = h)
  }
  
  result <- tibble()
  
  if (file.exists(filepath)) {
     result <- read_excel(filepath)
  }
  result <- add_column(result, BillingPeriod = format(Sys.Date(), "%Y%m")) %>%
    select(
      Name,
      ConsumptionPartNumber,
      ReportedUnits,
      EnterpriseUnits,
      UnitOfMeasure,
      ConversionFactor,
      MeterCategory = "Meter Category",
      BillingPeriod
    ) 
  
  return(result)
}