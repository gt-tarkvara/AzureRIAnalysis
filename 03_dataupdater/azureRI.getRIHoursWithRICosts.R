azureRI.getRIHoursWithRICosts <- function(apiObj, billingPeriod, ...) {
  result <- azureRI.get("RIHoursWithRICosts_raw", apiObj = apiObj, billingPeriod = billingPeriod) %>%  
    group_by(InstanceId, Date, SubscriptionGuid, ConsumptionMeter) %>%
    summarise(
      RIHours=sum(ConsumedQuantity, na.rm = T), 
      RIRate=mean(UsedRIRate,na.rm = T), 
      RICost=sum(RICost, na.rm = T)
    )
}