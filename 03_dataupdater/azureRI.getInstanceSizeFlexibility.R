if(!exists("azureRI", mode="function")) source("azureRI.R")

# TODO: add opportunity to retrieve snapshot made for specific billing period
azureRI.getInstanceSizeFlexibility <- function() {
  url <- "https://docs.microsoft.com/en-us/azure/virtual-machines/windows/reserved-vm-instance-size-flexibility"  

  #h <- curl::new_handle()
  #curl::handle_setopt(h, http_version = 0L)
  
  webpage <- read_html(url)
  
  #webpage <- curl(url = url, handle = h)
  
  results <- webpage %>% html_nodes('main')
  
  tables <- results[1] %>% html_nodes("table") %>% html_table()
  
  flexibleTables <- do.call("rbind", tables)
  flexibleTables <- as_tibble(flexibleTables)
  flexibleTables <- add_column(flexibleTables, billingperiod = format(Sys.Date(), "%Y%m"))
  
  return(flexibleTables)
}
