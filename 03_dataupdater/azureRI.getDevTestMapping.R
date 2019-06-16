azureRI.getDevTestMapping <- function(apiObj, billingPeriod, reload=F, ...) {
  
  # Trouble with those is that they get copied... Should get just a reference... Optimize !LATER!
  priceSheet <- azureRI.get("PriceList", obj = apiObj, billingPeriod = billingPeriod, reload=reload, ...)
  friendlyServiceNames <- azureRI.get("FriendlyServiceNames", apiObj = apiObj, billingPeriod = billingPeriod, reload=reload, ...)
  
  devTestMapping <- friendlyServiceNames %>%
    filter(MeterCategory == "Virtual Machines")
  
  devTestMapping <- left_join(x = devTestMapping, y = priceSheet, by = c("ConsumptionPartNumber" = "PartNumber"))
  
  
  devTestMapping_B <- devTestMapping %>%
    mutate(NameWithoutDevTest = str_replace(Name, "Dev/Test - ","")) %>%
    filter(str_detect(Name, "Dev/Test") ) %>%
    select(
      NameWithoutDevTest,
      DevTestPrice = UnitPrice,
      DevTestPartNumber = ConsumptionPartNumber,
      DevTestMeterId = MeterId
    )
  
  devTestMapping <- left_join(x=devTestMapping_B, y=devTestMapping , by=c("NameWithoutDevTest" = "Name")) %>%
    select(
      Name = NameWithoutDevTest,
      FullPricePartNumber = ConsumptionPartNumber,
      FullPriceMeterId = MeterId,
      FullPrice = UnitPrice,
      DevTestPartNumber,
      DevTestPrice,
      DevTestMeterId
    )
  
  rm(devTestMapping_B)
  return(devTestMapping)
}