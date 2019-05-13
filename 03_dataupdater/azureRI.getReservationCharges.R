
if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getReservationCharges <- function(obj = NULL, startdate = Sys.Date() - 3*365, enddate = Sys.Date()) {
  
  if (is.null(obj)) {
    obj <- azureRI.default
  }
  
  if (!is(obj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  query <- paste0("reservationcharges?startDate=", startdate ,"&endDate=", enddate)
  
  result <- azureRI.CallBillingApi(obj, version = "v3", query = query )
  
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
  ) %>%
    mutate(
      baseHourRate = if_else(term=="P1Y",(amount/(365*24))/quantity, (amount/(3*365*24))/quantity)
    ) %>%
    filter(
      eventType == "Purchase"
    )
  
  
  return(result)
}