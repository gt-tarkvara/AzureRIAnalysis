if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getUsageDetails <- function(obj = NULL, billingPeriod = NULL) {
  
  if (is.null(obj)) {
    obj <- azureRI.default
  }
  
  if (!is(obj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  # previous month if not specified
  if (is.null(billingPeriod)) {
    billingPeriod <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y%m")
  }
  
  
  
  q <- paste0("/usagedetails/download?billingPeriod=", billingPeriod)
  
  filepath <- paste(tempdir(), paste0("usagedetails-", billingPeriod, ".csv"), sep = .Platform$file.sep)
  
  print(filepath)
  
  result <- azureRI.CallBillingApi(obj, version = "v3", query = q, filepath = filepath, reload = FALSE )
  
  if (is.na(result)) {
    return(tibble())
  }
  
  result <- read_csv(filepath)
  
  return(result)
}