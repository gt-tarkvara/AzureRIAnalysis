#' AzureRI get billingData
#'
#' This function tries to retrieve information about your Azure RI instances for billing period
#' Function is meant to be used through azureRI.get()
#' @param apiObj API Object created using azureRI() constructor function, defaults to NULL
#' @param billingPeriod Billing period in form of YYYYMM, defaults to last month
#' @keywords billingData
#' @export
#' @examples
#' azureRI.getBillingData(apiObj, billingPeriod="201901")

azureRI.getBillingData <- function(apiObj=NULL, billingPeriod=NULL, ...) {
  
  usageDetails <- azureRI.get("UsageDetails", apiObj = apiObj, billingPeriod = billingPeriod, ...)  
  riHoursWithRICosts <- azureRI.get("RIHoursWithRICosts", apiObj = apiObj, billingPeriod = billingPeriod, ...)
  priceSheet <- azureRI.get("PriceList", apiObj = apiObj, billingPeriod = billingPeriod, ...)
  devTestMapping <- azureRI.get("DevTestMapping", apiObj = apiObj, billingPeriod = billingPeriod, ...)
  
  vmDetails <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    filter(
      Product != "Reservation-Base VM" & Product != "VM RI - Compute" 
    ) %>%
    group_by(InstanceId, MeterId, SubscriptionName, Product, MeterSubCategory, MeterCategory, UnitOfMeasure, PartNumber, 
             ConversionFactor, 
             SubscriptionGuid) %>%
    count() %>%
    select(
      -n
    )
  
  
  instanceNames <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    group_by(SubscriptionGuid, SubscriptionName, Date, InstanceId) %>%
    count() %>%
    select(
      -n
    )
  
  instanceNames <- left_join(x=instanceNames, y=vmDetails, by=c("SubscriptionGuid"="SubscriptionGuid", "InstanceId"="InstanceId")) %>%
    rename(SubscriptionName = SubscriptionName.x) %>%
    select(
      -SubscriptionName.y
    )
  
  
  
  usageDetailsWithEmptyRows <- bind_rows(usageDetails, instanceNames) %>%
    rename(ExtendedCost = Cost)
  
  rm(usageDetails)
  rm(instanceNames)
  rm(vmDetails)
  
  
  
  # === BillingData
  billingData <- usageDetailsWithEmptyRows %>%
    group_by(Date, InstanceId, MeterId, SubscriptionGuid, SubscriptionName, Product, MeterSubCategory, MeterCategory, UnitOfMeasure, PartNumber,
             ConversionFactor
    ) %>%
    summarise(
      ConsumedUnits = sum(ConsumedUnits, na.rm = T),
      EffectiveRate = mean(EffectiveRate, na.rm = T),
      ExtendedCost = sum(ExtendedCost, na.rm =T )
    ) %>%
    mutate(
      EffectiveRate = ifelse(is.na(EffectiveRate), 0, EffectiveRate)
    )
  
  billingData <- left_join(x=billingData, y=riHoursWithRICosts, by=c("Date"="Date", "InstanceId"="InstanceId", "MeterId"="ConsumptionMeter")) %>%
    select(
      -SubscriptionName.y,
      -SubscriptionGuid.y,
    ) %>%
    rename(
      SubscriptionGuid = SubscriptionGuid.x,
      SubscriptionName = SubscriptionName.x
    )
  
  billingData <- left_join(x=billingData, y=priceSheet, by=c("PartNumber"="PartNumber")) %>%
    mutate(
      ConsumedUnitsCoveredByRI = if_else(is.na(RIHours), 0, RIHours/ConversionFactor ),
      ConsumedUnitsCoveredByRIFullPrice = ConsumedUnitsCoveredByRI*UnitPrice,
      CostSavingsFromRI = if_else(is.na(ConsumedUnitsCoveredByRIFullPrice-RICost),0, ConsumedUnitsCoveredByRIFullPrice-RICost)
    ) %>%
    rename(
      CostUsageFromRI = RICost,
      RIHourRate = RIRate,
      RIHoursUsed = RIHours
    )
  
  billingData <- left_join(x=billingData, y=devTestMapping, by=c("PartNumber"="DevTestPartNumber")) %>%
    mutate(
      DevTestConsumptionCostWithFullPrice = FullPrice*ConsumedUnits,
      CostSavingsFromDevTest = if_else(is.na(DevTestConsumptionCostWithFullPrice),0, DevTestConsumptionCostWithFullPrice-ExtendedCost),
      CostSavingsTotal = CostSavingsFromRI + CostSavingsFromDevTest
    )
}