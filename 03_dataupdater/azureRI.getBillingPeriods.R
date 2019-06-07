
if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getBillingPeriods <- function(apiObj = NULL, billingPeriod=NULL, ...) {
  
  if (is.null(apiObj)) {
    apiObj <- azureRI.default
  }
  
  if (!is(apiObj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  # check if periods exist in obj Envrionment
  if (exists("billingPeriods", envir = apiObj$env)) {
    billingPeriods <- get("billingPeriods", envir = apiObj$env)
  
    if (!is.null(billingPeriods)) {
      return(billingPeriods)
    }
  }
  
  
  #if (!is.null(apiObj$billingPeriods)) {
  #  return(apiObj$billingPeriods)
  #}

  
  
  
  result <- azureRI.CallBillingApi(apiObj, version = "v2", query = "billingperiods" ) 
  
  if (is.na(result)) {
    return(tibble())
  }
  
  result <- tryCatch(
    {
      as.tibble(fromJSON(result))
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
  apiObj$env$billingPeriods <- result
  
  
  return(result)
}