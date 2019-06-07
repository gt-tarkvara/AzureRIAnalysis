#' AzureRI get RI Hours Utilization
#'
#' This function tries to retrieve information about your Azure RI instances for billing period
#' Function is meant to be used through azureRI.get()
#' @param apiObj API Object created using azureRI() constructor function, defaults to NULL
#' @param billingPeriod Billing period in form of YYYYMM, defaults to last month
#' @keywords RIHours Azure Utilization
#' @export
#' @examples
#' azureRI.getRIHoursUtilization(apiObj, billingPeriod="201901")

azureRI.getRIHoursUtilization <- function(apiObj=NULL, billingPeriod=NULL, ...) {
  
  reservationCharge <- azureRI.get("ReservationCharges", apiObj = apiObj, billingPeriod = billingPeriod, ...)  
  riHoursWithRICosts_raw <- azureRI.get("RIHoursWithRICosts_raw", apiObj = apiObj, billingPeriod = billingPeriod, ...)  
  
  riHoursUtilization <- riHoursWithRICosts_raw %>%  
    group_by(InstanceId, Date, SubscriptionGuid, ConsumptionMeter, ReservationOrderId, RealArmSkuName) %>%
    summarise(
      RIHours=sum(ConsumedQuantity, na.rm = T), 
      RIRate=mean(UsedRIRate,na.rm = T), 
      RICost=sum(RICost, na.rm = T)
    )
  
  riHoursUtilization <- left_join(x=riHoursUtilization, y=reservationCharge, 
                                  by=c("SubscriptionGuid"="PurchasingSubscriptionGuid", "ReservationOrderId"="ReservationOrderId")) %>%
    mutate(
      TotalHoursAvailable = Quantity*24,
      TotalRICommitmentAvailable = TotalHoursAvailable*BaseHourRate,
      AvailableRIHoursUsage = RIHours/TotalHoursAvailable,
      AvailableRICommitmentUsage = RICost/TotalRICommitmentAvailable
    ) %>%
    select(
      Amount
      ,AvailableRICommitmentUsage
      ,AvailableRIHoursUsage
      ,BaseHourRate
      ,ConsumptionMeter
      ,Date
      ,InstanceId
      ,PurchasingSubscriptionName
      ,Quantity
      ,ReservationOrderId
      ,RICost
      ,RIHours
      ,RIRate
      ,SubscriptionGuid
      ,TotalHoursAvailable
      ,TotalRICommitmentAvailable
    )
}