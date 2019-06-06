if(!exists("azureRI", mode="function")) source("azureRI.R")

# TODO: add opportunity to retrieve snapshot made for specific billing period
azureRI.getInstanceSizeFlexibility <- function(apiObj, billingPeriod, ...) {
  
  
  # Documented url with instance sizes
  iurl <- "https://docs.microsoft.com/en-us/azure/virtual-machines/windows/reserved-vm-instance-size-flexibility"  

  #h <- curl::new_handle()
  #curl::handle_setopt(h, http_version = 0L)
  
  h <- curl::new_handle()
  curl::handle_setopt(h, http_version = 0L)
  
  temp_dir <- apiObj$cachedir
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = T)
  }
  
  filepath <- paste(temp_dir, paste0("instancesizeflexibility-", billingPeriod, ".html"), sep = .Platform$file.sep)
  
  
  if (!file.exists(filepath)) {
    req <- curl::curl_download(iurl, filepath, mode = "wb+", handle = h)
  }
  
  
  
  webpage <- read_html(filepath)
  
  #webpage <- curl(url = url, handle = h)
  
  results <- webpage %>% html_nodes('main')
  
  tables <- results[1] %>% html_nodes("table") %>% html_table()
  
  flexibleTables <- do.call("rbind", tables)
  flexibleTables <- as_tibble(flexibleTables)
  flexibleTables <- add_column(flexibleTables, BillingPeriod = format(Sys.Date(), "%Y%m"))
  
  return(flexibleTables)
}
