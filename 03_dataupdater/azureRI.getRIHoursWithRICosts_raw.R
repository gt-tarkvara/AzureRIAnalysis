
azureRI.getRIHoursWithRICosts_raw <- function(apiObj, billingPeriod, ...) {
  
  usageDetails <- azureRI.get("UsageDetails", apiObj = apiObj, billingPeriod = billingPeriod, ...)  
  reservationCharge <- azureRI.get("ReservationCharges", apiObj = apiObj, billingPeriod = billingPeriod, ...)  
  instanceSizeFlexibility <- azureRI.get("InstanceSizeFlexibility", apiObj = apiObj, billingPeriod = billingPeriod, ...)
  
  
  # Augment usageDetails
  usageDetails <- usageDetails %>%
    mutate(
      VMName = ifelse(
        MeterCategory == "Virtual Machines" & is.na(VMName),
        basename(InstanceId),
        VMName
      )  
    )
  
  riHoursWithRICosts_raw <- usageDetails %>%
    select(
      -AccountName,
      -Cost,
      -ConsumedService,
      -MeterCategory,
      -MeterName,
      -MeterRegion,
      -MeterSubCategory,
      -ResourceGroup,
      -ResourceRate,
      -ServiceInfo1,
      -ServiceInfo2,
      -Tags,
      -UnitOfMeasure,
      -ResourceGuid,
      -OfferId,
      -ChargesBilledSeparately,
      -Location,
      -ServiceName,
      -ServiceTier
    ) %>%
    filter(
      Product == "Reservation-Base VM" | Product == "VM RI - Compute" 
    ) %>%
    filter(
      !is.na(ConsumptionMeter) # sometimes missing. I think should report this also as anomaly..
    ) %>%
    rename(
      RealArmSkuName = ServiceType 
    )


  riHoursWithRICosts_raw <-left_join(x = riHoursWithRICosts_raw, y = reservationCharge, 
                                     by = c("SubscriptionGuid"="PurchasingSubscriptionGuid",
                                            "ReservationOrderId"="ReservationOrderId")) %>%
    select(
      -PurchasingEnrollment,
      -Term,
      -Region,
      -PurchasingSubscriptionName,
      -AccountName,
      -AccountOwnerEmail,
      -DepartmentName,
      -CostCenter,
      -CurrentEnrollment,
      -EventDate,
      -Description,
      -EventType,
      -Quantity,
      -Amount,
      -Currency,
      -ReservationOrderName
    ) 

  riHoursWithRICosts_raw <- left_join(x = riHoursWithRICosts_raw, y = instanceSizeFlexibility, by = c("ArmSkuName"="Size")) %>%
    select(
      -BillingPeriod.x, 
      -BillingPeriod.y 
    ) %>%
    rename(Maximum.Ratio = Ratio)

  riHoursWithRICosts_raw <- left_join(x = riHoursWithRICosts_raw, y = instanceSizeFlexibility, by = c("RealArmSkuName"="Size")) %>%
    rename(Actual.Ratio = Ratio) %>%
    mutate(
      UsedRIRate = BaseHourRate*(Actual.Ratio/Maximum.Ratio),
      RICost = ConsumedQuantity * UsedRIRate
    ) 

  return (riHoursWithRICosts_raw)

}