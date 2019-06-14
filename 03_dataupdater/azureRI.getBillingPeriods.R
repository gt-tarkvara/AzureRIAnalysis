
if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getBillingPeriods <- function(apiObj = NULL, billingPeriod=NULL, ...) {
  
  if (is.null(apiObj)) {
    apiObj <- azureRI.default
  }
  
  if (!is(apiObj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  # check if periods exist in obj Envrionment
  if (exists("BillingPeriods", envir = apiObj$env)) {
    billingPeriods <- get("BillingPeriods", envir = apiObj$env)
  
    if (!is.null(billingPeriods)) {
      return(billingPeriods)
    }
  }
  
  result <- azureRI.CallBillingApi(apiObj, version = "v2", query = "billingperiods" ) 
  
  if (is.na(result)) {
    return(tibble())
  }
  
  result <- tryCatch(
    {
      as_tibble(fromJSON(result))
    },
    error = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    },
    warning = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    }
  )
  
  # set env
  apiObj$env$BillingPeriods <- result
  
  
  return(result)
}