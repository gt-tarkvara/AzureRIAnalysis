
azureRI.getRIHoursWithRICosts_raw <- function(apiObj, billingPeriod, ...) {
  
  usageDetails <- azureRI.get("UsageDetails", apiObj = apiObj, billingPeriod = billingPeriod, ...)  
  reservationCharge <- azureRI.get("ReservationCharges", apiObj = apiObj, billingPeriod = billingPeriod, ...)  
  instanceSizeFlexibility <- azureRI.get("InstanceSizeFlexibility", apiObj = apiObj, billingPeriod = billingPeriod, ...)
  
  
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
      -SubscriptionName,
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
    rename(
      RealArmSkuName = ServiceType 
    )


  riHoursWithRICosts_raw <-left_join(x = riHoursWithRICosts_raw, y = reservationCharge, 
                                     by = c("SubscriptionGuid"="purchasingSubscriptionGuid",
                                            "ReservationOrderId"="reservationOrderId")) %>%
    select(
      -purchasingEnrollment,
      -term,
      -region,
      -purchasingSubscriptionName,
      -accountName,
      -accountOwnerEmail,
      -departmentName,
      -costCenter,
      -currentEnrollment,
      -eventDate,
      -description,
      -eventType,
      -quantity,
      -amount,
      -currency,
      -reservationOrderName
    ) 

  riHoursWithRICosts_raw <- left_join(x = riHoursWithRICosts_raw, y = instanceSizeFlexibility, by = c("armSkuName"="Size")) %>%
    select(
      -BillingPeriod.x, 
      -BillingPeriod.y 
    ) %>%
    rename(Maximum.Ratio = Ratio)

  riHoursWithRICosts_raw <- left_join(x = riHoursWithRICosts_raw, y = instanceSizeFlexibility, by = c("RealArmSkuName"="Size")) %>%
    rename(Actual.Ratio = Ratio) %>%
    mutate(
      usedRIRate = baseHourRate*(Actual.Ratio/Maximum.Ratio),
      RICost = ConsumedQuantity * usedRIRate
    ) 

  return (riHoursWithRICosts_raw)

}