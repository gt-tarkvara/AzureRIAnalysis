
if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getBillingPeriods <- function(obj = NULL) {
  
  if (is.null(obj)) {
    obj <- azureRI.default
  }
  
  if (!is(obj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  

  result <- azureRI.CallBillingApi(obj, version = "v2", query = "billingperiods" ) 
  
  if (is.na(result)) {
    return(tibble())
  }
  
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
  )
  
  return(result)
}