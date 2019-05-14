
if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getPriceList <- function(obj = NULL, billingPeriod = NULL) {
  
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
  
  # check if billingperiod is in acceptable limits
  periods <- azureRI.getBillingPeriods(obj)
  
  if (!(billingPeriod %in% periods[,1])) {
    warning("Invalid billing period specified", immediate. = TRUE)
    return(tibble())
  }
  
  temp_dir <- obj$cachedir
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = T)
  }
  
  query <- paste0("billingPeriods/", billingPeriod, "/pricesheet")
  
  filepath <- paste(temp_dir, paste0("pricesheet-", billingPeriod, ".json"), sep = .Platform$file.sep)
  
  #print(filepath)
  
  result <- azureRI.CallBillingApi(obj, version = "v2", query = query, filepath = filepath, reload = FALSE )
  
  if (is.na(result)) {
    return(tibble())
  }
  
  margin <- obj$margin
  
  result <- tryCatch(
    {
      fromJSON(result)
    },
    error = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    },
    warning = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    }
  ) %>%
    mutate(
      unitPrice = unitPrice * margin
    )
  
  return(result)
}