
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
  
  query <- paste0("billingPeriods/", billingPeriod, "/pricesheet")
  
  result <- azureRI.CallBillingApi(obj, version = "v2", query = query )
  
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
  
#  %>%
#    mutate(
#      baseHourRate = if_else(term=="P1Y",(amount/(365*24))/quantity, (amount/(3*365*24))/quantity)
#    ) %>%
#    filter(
#      eventType == "Purchase"
#    )
  
  
  return(result)
}