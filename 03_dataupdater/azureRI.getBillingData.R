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
  friendlyServiceNames <- azureRI.get("FriendlyServiceNames", apiObj = apiObj, billingPeriod = billingPeriod, ...)
  
  
  # Augment usageDetails with Hack
  usageDetails <- usageDetails %>%
    mutate( RILinkingMeterId = ifelse(
      is.na(ConsumptionMeter),
      ifelse(
        str_detect(Product, " Windows"),
        NA,
        MeterId),
      ConsumptionMeter
    ))
  
  
  # All instance names
  instanceNamesAll <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    group_by(SubscriptionGuid, SubscriptionName, InstanceId) %>%
    count() %>%
    select(
      -n
    )
  
  
  #===========================================================
  # Construct vmDetails for cases there are only RI usage
  #===========================================================
  
  # Regular vm compute usage
  instanceNamesWithoutRI <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    filter(
      (Product != "Reservation-Base VM" & Product != "VM RI - Compute") 
    ) %>%
    group_by(SubscriptionGuid, SubscriptionName, InstanceId) %>%
    count() %>%
    select(
      -n
    )
  
  # Lookup subcategory data from all usage details - this information is not available in other metadata tables :(
  meterSubCategoryLookup <- usageDetails %>% 
    group_by(MeterCategory, MeterSubCategory, Product, PartNumber) %>% 
    count() %>%
    select( -n )
  
  # filter out those instances which have only RI usage
  instanceNamesOnlyRI <- anti_join(x=instanceNamesAll, y=instanceNamesWithoutRI, by=c("SubscriptionGuid"="SubscriptionGuid", "InstanceId"="InstanceId"))
  
  # construct data set
  vmDetailsRIOnly <- left_join(x=instanceNamesOnlyRI, y=usageDetails, 
                               by=c("SubscriptionGuid"="SubscriptionGuid", 
                                    "SubscriptionName"="SubscriptionName", 
                                    "InstanceId"="InstanceId")) %>%
    filter(MeterCategory=="Virtual Machines") %>% 
    left_join(y = priceSheet, by=c("ConsumptionMeter"="MeterId")) %>%
    left_join(y = friendlyServiceNames, by=c("PartNumber.y"="ConsumptionPartNumber")) %>% 
    left_join(y = meterSubCategoryLookup, by=c("PartNumber.y"="PartNumber")) %>%
    mutate(
      ConsumedUnits = 0,
      ConversionFactor = as.numeric(ConversionFactor.y),
      MeterId = ConsumptionMeter
    ) %>%
    select(
      SubscriptionGuid,
      SubscriptionName,
      Date,
      InstanceId,
      Cost,
      MeterId,
      RILinkingMeterId = ConsumptionMeter,
      MeterCategory = MeterCategory.x,
      MeterSubCategory = MeterSubCategory.y,
      PartNumber = PartNumber.y,
      Product = Name.y,
      UnitOfMeasure = UnitsOfMeasurePriceList,
      ConversionFactor,
      EffectiveRate = UnitPrice,
      ConsumedUnits
    ) 
  
  #===========================================================
  # Construct vmDetails for cases there are both VM regular 
  # and RI usage
  #===========================================================
  
  # RILinkingMeterId for RI compute metering. 
  instanceId_Name_MeterId_Date <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    filter(
      (Product == "Reservation-Base VM" | Product == "VM RI - Compute") 
    ) %>%
    group_by(SubscriptionGuid, SubscriptionName, InstanceId, Date, RILinkingMeterId) %>%
    count() %>%
    select(
      -n
    ) %>%
    filter(
      !is.na(RILinkingMeterId)
    ) 
  
  
  # instance names by subscription and date
  instanceNames <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    group_by(SubscriptionGuid, SubscriptionName, Date, InstanceId) %>%
    count() %>%
    select(
      -n
    ) %>%
    # Augment by RILinkingMeterId from RI meters, it is used when RILinkingMeterId for VM is empty.
    left_join(y = instanceId_Name_MeterId_Date, by=c(
      "SubscriptionGuid"="SubscriptionGuid", 
      "SubscriptionName"="SubscriptionName", 
      "Date"="Date", 
      "InstanceId"="InstanceId")) %>%
    rename(RIMeterLinkingIdFromRIUsage=RILinkingMeterId)
  
  # For every VM, there should be at least 1 row per month..
  vmDetails <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    filter(
      (Product != "Reservation-Base VM" & Product != "VM RI - Compute") 
    ) %>%
    group_by(InstanceId, MeterId, SubscriptionName, Product, MeterSubCategory, MeterCategory, UnitOfMeasure, PartNumber, 
             ConversionFactor, 
             SubscriptionGuid, RILinkingMeterId) %>%
    count() %>%
    select(
      -n
    )
  
  # Add date part
  vmDetails2 <- instanceNames %>% 
    left_join(vmDetails, 
              by=c("SubscriptionName"="SubscriptionName",
                   "SubscriptionGuid"="SubscriptionGuid",
                   "InstanceId"="InstanceId")) %>%
    filter(!is.na(MeterId)) %>%
    mutate(
      RILinkingMeterId = ifelse(is.na(RILinkingMeterId), RIMeterLinkingIdFromRIUsage, RILinkingMeterId)
    ) %>%
    select(
      -RIMeterLinkingIdFromRIUsage
    )
  
  # combine vm data
  vmDetails3 <- bind_rows(vmDetails2, vmDetailsRIOnly) 
  
  # combine vmdata with usage info
  usageDetailsWithEmptyRows <- bind_rows(usageDetails, vmDetails3) %>%
    rename(ExtendedCost = Cost) %>% filter(
      Product != "Reservation-Base VM" & Product != "VM RI - Compute" &
        MeterCategory != "Virtual Machines Licenses" # Huh, this caused double join. 
    )
  
  
  billingData <- usageDetailsWithEmptyRows %>%
    group_by(Date, InstanceId, MeterId, SubscriptionGuid, SubscriptionName, Product, MeterSubCategory, MeterCategory, UnitOfMeasure, PartNumber,
             ConversionFactor, RILinkingMeterId
    ) %>%
    summarise(
      ConsumedUnits = sum(ConsumedUnits, na.rm = T),
      EffectiveRate = mean(EffectiveRate, na.rm = T),
      ExtendedCost = sum(ExtendedCost, na.rm =T )
    ) %>%
    mutate(
      EffectiveRate = ifelse(is.na(EffectiveRate), 0, EffectiveRate)
    ) %>% 
    left_join(y=riHoursWithRICosts, 
              by=c("SubscriptionGuid"="SubscriptionGuid", "Date"="Date", "InstanceId"="InstanceId", "RILinkingMeterId"="ConsumptionMeter")) %>%
    select(
      -SubscriptionName.y
    ) %>%
    rename(
      SubscriptionName = SubscriptionName.x
    )
  
  
  billingData <- left_join(x=billingData, y=priceSheet, by=c("PartNumber"="PartNumber")) %>%
    mutate(
      ConsumedUnitsCoveredByRI = if_else(is.na(RIHours), 0, RIHours/ConversionFactor ),
      ConsumedUnitsCoveredByRIFullPrice = ConsumedUnitsCoveredByRI*UnitPrice,
      CostSavingsFromRI = if_else(is.na(ConsumedUnitsCoveredByRIFullPrice-RICost),0, ConsumedUnitsCoveredByRIFullPrice-RICost)
    ) %>%
    rename(
      MeterId = MeterId.x,
      CostUsageFromRI = RICost,
      RIHourRate = RIRate,
      RIHoursUsed = RIHours
    ) %>%
    select(
      -MeterId.y
    )
  
  billingData <- left_join(x=billingData, y=devTestMapping, by=c("PartNumber"="DevTestPartNumber")) %>%
    mutate(
      DevTestConsumptionCostWithFullPrice = FullPrice*ConsumedUnits,
      CostSavingsFromDevTest = if_else(is.na(DevTestConsumptionCostWithFullPrice),0, DevTestConsumptionCostWithFullPrice-ExtendedCost),
      CostSavingsTotal = CostSavingsFromRI + CostSavingsFromDevTest
    )
}