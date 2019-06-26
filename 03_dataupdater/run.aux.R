source('azureRI.R')

billingPeriod <- Sys.getenv("AZURERI_BILLINGPERIOD")

if (is.null(billingPeriod)) {
  billingPeriod <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y%m")
}

# Load data
priceSheet <- azureRI.get("PriceList", azureRI.default, row.names = "BusinessKey", billingPeriod = billingPeriod, reload=T) 
friendlyServiceNames <- azureRI.get("FriendlyServiceNames", azureRI.default, row.names = "BusinessKey", billingPeriod = billingPeriod, reload=T )
instanceSizeFlexibility <- azureRI.get("InstanceSizeFlexibility", azureRI.default, row.names = "BusinessKey", billingPeriod = billingPeriod, reload=T)

# Update data
azureRI.set(what="PriceList", apiObj = azureRI.default, data = priceSheet, billingPeriod = billingPeriod)
azureRI.set(what="FriendlyServiceNames", apiObj = azureRI.default, data = friendlyServiceNames, billingPeriod = billingPeriod)
azureRI.set(what="InstanceSizeFlexibility", apiObj = azureRI.default, data = instanceSizeFlexibility, billingPeriod = billingPeriod)
