
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
  return(fromJSON(result))
}