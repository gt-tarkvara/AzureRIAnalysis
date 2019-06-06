azureRI.getDevTestMapping <- function(apiObj, billingPeriod, ...) {
  
  # Trouble with those is that they get copied... Should get just a reference... Optimize !LATER!
  priceSheet <- azureRI.get("PriceList", obj = apiObj, billingPeriod = billingPeriod)
  friendlyServiceNames <- azureRI.get("FriendlyServiceNames", apiObj = apiObj, billingPeriod = billingPeriod)
  
  devTestMapping <- friendlyServiceNames %>%
    filter(MeterCategory == "Virtual Machines")
  
  devTestMapping <- left_join(x = devTestMapping, y = priceSheet, by = c("ConsumptionPartNumber" = "PartNumber"))
  
  devTestMapping_B <- devTestMapping %>%
    mutate(NameWithoutDevTest = str_replace(Name, "Dev/Test - ","")) %>%
    filter(str_detect(Name, "Dev/Test") ) %>%
    select(
      NameWithoutDevTest,
      DevTestPrice = UnitPrice,
      DevTestPartNumber = ConsumptionPartNumber
    )
  
  devTestMapping <- left_join(x=devTestMapping_B, y=devTestMapping , by=c("NameWithoutDevTest" = "Name")) %>%
    select(
      Name = NameWithoutDevTest,
      FullPricePartNumber = ConsumptionPartNumber,
      FullPrice = UnitPrice,
      DevTestPartNumber,
      DevTestPrice
    )
  
  rm(devTestMapping_B)
  return(devTestMapping)
}