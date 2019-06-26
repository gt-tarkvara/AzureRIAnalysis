source('azureRI.R')

billingPeriod <- Sys.getenv("AZURERI_BILLINGPERIOD")

if (is.null(billingPeriod)) {
  billingPeriod <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y%m")
}

# load data
billingData <- azureRI.get(what="BillingData", apiObj = azureRI.default, row.names = "BusinessKey", billingPeriod = billingPeriod, reload=T)
riHoursUtilization <- azureRI.get(what="RIHoursUtilization", apiObj = azureRI.default, row.names = "BusinessKey", billingPeriod = billingPeriod, reload=T)

# save data
azureRI.set(what="BillingData", apiObj = azureRI.default, data = billingData, billingPeriod = billingPeriod)
azureRI.set(what="RIHoursUtilization", apiObj = azureRI.default, data = riHoursUtilization, billingPeriod = billingPeriod)