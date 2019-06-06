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

azureRI.getBillingData <- function(apiObj=NULL, billingPeriod=NULL) {
  
# === BillingData
#billingData <- usageDetailsWithEmptyRows %>%
billingData <- azureRI.get("usageDetailsWithEmptyRows", apiObj, billingPeriod) %>%
  group_by(Date, InstanceId, MeterId, SubscriptionName, Product, MeterSubCategory, MeterCategory, UnitOfMeasure, PartNumber,
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

billingData <- left_join(x=billingData, y=riHoursWithRICosts, by=c("Date"="Date", "InstanceId"="InstanceId", "MeterId"="ConsumptionMeter")) 

billingData <- left_join(x=billingData, y=priceSheet, by=c("PartNumber"="partNumber")) %>%
  mutate(
    ConsumedUnitsCoveredByRI = if_else(is.na(RIHours), 0, RIHours/ConversionFactor ),
    ConsumedUnitsCoveredByRIFullPrice = ConsumedUnitsCoveredByRI*unitPrice,
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