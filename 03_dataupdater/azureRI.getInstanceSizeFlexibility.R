library(rvest)


azureRI.getInstanceSizeFlexibility <- function() {
  url <- "https://docs.microsoft.com/en-us/azure/virtual-machines/windows/reserved-vm-instance-size-flexibility"  

  #h <- curl::new_handle()
  #curl::handle_setopt(h, http_version = 0L)
  
  webpage <- read_html(url)
  
  #webpage <- curl(url = url, handle = h)
  
  results <- webpage %>% html_nodes('main')
  
  tables <- results[1] %>% html_nodes("table") %>% html_table()
  
  flexibleTables <- do.call("rbind", tables)
  
  return(flexibleTables)
}
