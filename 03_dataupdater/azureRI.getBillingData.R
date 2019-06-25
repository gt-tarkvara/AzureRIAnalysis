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
  reservationCharges <- azureRI.get("ReservationCharges", apiObj = apiObj, billingPeriod = billingPeriod, ...)
  
  # All instance names
  instanceNamesAll <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines") %>%
    group_by(SubscriptionGuid, SubscriptionName, InstanceId) %>%
    count() %>%
    select(
      -n
    )
  
  # check1
  ck1 <- nrow(usageDetails)
  
  # Augment all Virtual Machine records to have VMName filled
  usageDetails <- usageDetails %>%
    mutate(
      VMName = ifelse(
        MeterCategory == "Virtual Machines" & is.na(VMName),
        basename(InstanceId),
        VMName
      )  
    )
  
  # lets get all RI usage
  riVMinstances <- usageDetails %>%
    filter(
      (Product == "Reservation-Base VM" | Product == "VM RI - Compute") 
    )
  
  # vm-s
  vmInstances <- usageDetails %>%
    filter(MeterCategory == "Virtual Machines" &
             (Product != "Reservation-Base VM" & Product != "VM RI - Compute"))
  
  # remove all VM rows from usageDetails, less calculation and matching to be done.
  usageDetails <- usageDetails %>%
    filter(
      MeterCategory != "Virtual Machines"
    )
  
  # check 2
  ck2 <- nrow(riVMinstances) + nrow(vmInstances) + nrow(usageDetails)
  
  if (ck1 != ck2) {
    warn("Row count does not match! (usageDetails)", immediate=T)
  }
  
  #check 3
  ck3 <- nrow(vmInstances)
  
  # all vm-s where consumptionmeterid is not na and is different from meterid
  vmInstancesWithDifferentConsumptionId <- vmInstances %>%
    filter(
      !is.na(ConsumptionMeter) 
    ) %>%
    filter(
      ConsumptionMeter != MeterId
    ) 
  
  # try to filter all those cases where there are multiple VM rows per day with same consumption meter
  vmFiltered <- 
    semi_join(
      x = vmInstances,
      y = vmInstancesWithDifferentConsumptionId,
      by = c("InstanceId"="InstanceId",
             "Date"="Date",
             "VMName"="VMName")
    ) %>% group_by(
      InstanceId,
      Date,
      VMName
    ) %>%
    count() %>%
    filter(
      n>1
    )
  
  # extract those cases to separate tibble
  vmFilteredCases <- 
    semi_join(
      x=vmInstances,
      y=vmFiltered,
      by=c(
        "InstanceId"="InstanceId",
        "Date"="Date",
        "VMName"="VMName"
      )
    )
  
  # remove filtered cases from main vmInstances
  vmInstances <- vmInstances %>%
    anti_join(
      y=vmFiltered,
      by=c(
        "InstanceId"="InstanceId",
        "Date"="Date",
        "VMName"="VMName"
      )
    )
  
  # check 4
  ck4 <- nrow(vmFilteredCases) + nrow(vmInstances)
  
  if (ck3 != ck4) {
    warn("Row count does not match! (vmInstances)", immediate=T)
  }
  
  # set linking RI for regular cases
  vmInstances <- vmInstances %>%
    mutate(
      RILinkingId = ifelse(
        is.na(ConsumptionMeter),
        MeterId,
        ConsumptionMeter
      )
    )
  
  # set linking RI for filtered cases
  vmFilteredCases <- vmFilteredCases %>%
    mutate(
      RILinkingId = ConsumptionMeter
    )
  
  # join vm instances back together
  vmInstances <- bind_rows(vmInstances, vmFilteredCases)
  
  # check 5
  ck5 <- nrow(vmInstances)
  if (ck5 != ck4) {
    warn("Row count does not match! (vmInstances + vmFilteredCases)", immediate=T)
  }
  
  # Now try to construct base vm record for those cases where we have only RI row
  
  # First, Create some lookup tables
  
  # set up lookup for finding VM details
  meterSubCategoryLookup <- vmInstances %>% 
    group_by(MeterCategory, MeterSubCategory, Product, PartNumber) %>% 
    count() %>%
    select( -n )
  
  # We have consumption meter - we can look up product by it.
  # pricesheet without dev/test
  priceSheetWithoutDevTest <- priceSheet %>%
    left_join(y = friendlyServiceNames, by=c("PartNumber"="ConsumptionPartNumber", "BillingPeriod"="BillingPeriod")) %>% 
    filter(MeterCategory == "Virtual Machines") %>%
    rename(MeterName = Name) %>%
    filter(!str_detect(MeterName, "Dev/Test")) %>%
    left_join(
      y=meterSubCategoryLookup,
      by=c("PartNumber"="PartNumber",
           "MeterCategory"="MeterCategory")
    ) %>%
    mutate(
      # Data augmentation
      MeterSubCategory =
        ifelse(
          is.na(MeterSubCategory),
          str_replace(MeterName, "Virtual Machines ((.)* Series( Windows| Basic| Basic Windows)*){1} - .*", "\\1"),
          MeterSubCategory
        ),
      # Data augmentation
      Product =
        ifelse(
          is.na(Product),
          MeterName,
          Product
        )
    )
  
  
  # Windows keys
  priceSheetWindows <- priceSheetWithoutDevTest %>%
    filter(str_detect(MeterName, " Series Windows ")) %>%
    mutate(
      MeterNameWithoutWindows = str_replace(MeterName, " Windows", "")
    ) %>%
    select(
      MeterNameWithoutWindows,
      MeterNameWithWindows = MeterName,
      MeterIdWithWindows = MeterId
    ) %>%
    left_join(
      y = priceSheetWithoutDevTest,
      by = c(
        "MeterNameWithoutWindows" = "MeterName" 
      )
    ) %>%
    select(
      MeterNameWithoutWindows,
      MeterNameWithWindows,
      MeterIdWithWindows,
      MeterIdWithoutWindows = MeterId,
    )
  
  # Link lookup key back to pricesheet
  priceSheetLookup <- priceSheetWithoutDevTest %>%
    left_join(
      y = priceSheetWindows,
      by = c(
        "MeterName" = "MeterNameWithWindows"
      )
    ) %>%
    mutate(
      LookupMeterId=ifelse(
        is.na(MeterIdWithWindows),
        MeterId,
        MeterIdWithoutWindows
      )
    ) %>%
    select(
      MeterId,
      LookupMeterId
    ) %>%
    mutate(
      MeterId = as.character(MeterId),
      LookupMeterId = as.character(LookupMeterId)
    )
  
  
  
  # need to find actual consumption id for those bits where name says windows but actually base vm is different
  
  
  # All vm-s mentioned in RI, lets see how many have missing VM counterpart
  riMissing <-riVMinstances %>%
    select(
      # - key fields
      SubscriptionName,
      SubscriptionGuid,
      Date,
      InstanceId,
      VMName,
      ConsumptionMeter,
      # - fields we can take over from RI
      AccountName,
      ResourceGroup,
      ImageType,
      Location,
      OfferId,
      ServiceInfo,
      ServiceInfo1,
      ServiceInfo2,
      ServiceName,
      ServiceType,
      VCPUs,
      VMProperties
      
      
    ) %>%
    anti_join(
      y = vmInstances,
      by = c(
        "SubscriptionName"="SubscriptionName",
        "SubscriptionGuid"="SubscriptionGuid",
        "InstanceId"="InstanceId",
        "Date"="Date",
        "VMName"="VMName",
        "ConsumptionMeter"="RILinkingId"
      )
    ) %>%
    left_join(
      y = priceSheetLookup,
      by = c(
        "ConsumptionMeter"="MeterId"
      )
    ) %>%
    rename(
      MeterId=LookupMeterId
    ) %>%
    left_join(
      y = priceSheetWithoutDevTest,
      by = c(
        "MeterId"="MeterId"
      )) %>%
    # calculated fields
    mutate(
      ConversionFactor = as.numeric(ConversionFactor),
      ResourceRate = UnitPrice/ConversionFactor,
      EffectiveRate = UnitPrice,
      Cost = 0,
      ConsumedUnits = 0,
      ConsumedQuantity = 0,
      UnitOfMeasure = paste(ConversionFactor, UnitOfMeasure),
      RILinkingId = ConsumptionMeter
    ) %>%
    select(
      -UnitsOfMeasurePriceList,
      -UnitPrice,
      -CurrencyCode,
      -IncludedQuantity
    ) %>%
    # Required because RI rows can come from different ReservationCharges and thus per (meterId,InstanceId, VMName, Date) key 
    # there might be multiple RI rows, one for each reservationcharge RI was applied from
    unique() 
  
  
  
  # Join vminstances and generated instances
  vmInstancesAll <- bind_rows(vmInstances, riMissing)
  
  # check 6 -  if we are missing any vm rows for RI rows?
  ck6 <- riHoursWithRICosts %>%
    anti_join(
      y = vmInstancesAll,
      by = c(
        "SubscriptionName"="SubscriptionName",
        "SubscriptionGuid"="SubscriptionGuid",
        "Date"="Date",
        "InstanceId"="InstanceId",
        "VMName"="VMName",
        "ConsumptionMeter"="RILinkingId"
      )
    )
  
  if (nrow(ck6) > 0) {
    warn("STILL some unmapped RI rows", immediate=T)
  }
  
  # check 7
  ck7 <- nrow(vmInstancesAll)
  
  # prepare pricesheet
  priceSheetFullLookup <- priceSheet %>%
    select(
      PartNumber,
      UnitPrice
    )
  
  # prepare devtest price lookup
  devTestMappingLookup <- devTestMapping %>%
    select(
      DevTestPartNumber,
      FullPrice
    )
  
  # join RI rows and calculate RI savings
  vmInstancesAll <- vmInstancesAll %>%
    # Add unitprice
    left_join(
      y = priceSheetFullLookup,
      by = c(
        "PartNumber"="PartNumber"
      )
    ) %>%
    left_join(
      y = riHoursWithRICosts,
      by = c(
        "SubscriptionName"="SubscriptionName",
        "SubscriptionGuid"="SubscriptionGuid",
        "Date"="Date",
        "InstanceId"="InstanceId",
        "VMName"="VMName",
        "RILinkingId"="ConsumptionMeter"
      )
    ) %>%
    mutate(
      ConsumedUnitsCoveredByRI = if_else(is.na(RIHours), 0, RIHours/ConversionFactor ),
      ConsumedUnitsCoveredByRIFullPrice = ConsumedUnitsCoveredByRI*UnitPrice,
      CostSavingsFromRI = if_else(is.na(ConsumedUnitsCoveredByRIFullPrice-RICost),0, ConsumedUnitsCoveredByRIFullPrice-RICost),
      CostUsageFromRI = ifelse(is.na(RICost),0,RICost),
      RIHourRate = ifelse(is.na(RIRate),0,RIRate),
      RIHoursUsed = ifelse(is.na(RIHours),0,RIHours)
    ) %>%
    # DevTest savings calculation
    left_join(
      y=devTestMappingLookup, 
      by=c("PartNumber"="DevTestPartNumber")) %>%
    mutate(
      DevTestConsumptionCostWithFullPrice = FullPrice*ConsumedUnits,
      CostSavingsFromDevTest = if_else(is.na(DevTestConsumptionCostWithFullPrice),0, DevTestConsumptionCostWithFullPrice-Cost),
      CostSavingsTotal = CostSavingsFromRI + CostSavingsFromDevTest
    ) 
  
  
  # check 8
  ck8 <- nrow(vmInstancesAll)
  
  if (ck7 != ck8) {
    warn("Row count does not match! (vmInstancesAll after joining with RI data)", immediate=T)
  }
  
  # Put everything back together with usageData
  billingData <- bind_rows(usageDetails, vmInstancesAll)
  
  #return (billingData)
  
  # have to add groupby because some data is truly on multiple lines, thus foiling unique key attempts.
  billingData2 <- billingData %>%
    group_by(
      AccountName
      #,AppServicePlanUri
      ,BillingPeriod
      #,ChargesBilledSeparately
      #,ConsumedService
      #,ConsumptionMeter
      #,ConversionFactor
      ,Date
      #,DeploymentLabel
      ,EnterpriseUnits
      #,HostedServiceLabel
      #,ImageType # here is sometimes information if image has byol or not
      #,InfoField
      ,InstanceId
      ,Location
      ,MeterCategory
      #,MeteredService
      #,MeteredServiceType
      ,MeterId
      ,MeterName
      #,MeterRegion
      ,MeterSubCategory
      ,Name
      ,OfferId
      ,PartNumber
      ,Product
      ,ReportedUnits
      #,ReservationId
      #,ReservationOrderId
      ,ResourceGroup
      #,ResourceGuid
      
      #,RILinkingId
      #,ServiceInfo
      #,ServiceInfo1
      #,ServiceInfo2 # here is sometimes information if image has byod license or  not
      ,ServiceName
      ,ServiceTier
      ,ServiceType
      ,SubscriptionGuid
      ,SubscriptionName
      #,Tags
      ,UnitOfMeasure
      #,UsageResource
      #,UsageType
      ,VCPUs
      ,VMName
      #,VMProperties
    ) %>%
    summarize(
      EffectiveRate = mean(EffectiveRate, na.rm = T)
      ,ConsumedQuantity = sum(ConsumedQuantity, na.rm = T)
      ,ConsumedUnits = sum(ConsumedUnits, na.rm = T)
      ,ConsumedUnitsCoveredByRI = sum(ConsumedUnitsCoveredByRI, na.rm = T)
      ,ConsumedUnitsCoveredByRIFullPrice = sum(ConsumedUnitsCoveredByRIFullPrice, na.rm = T)
      ,Cost = sum(Cost, na.rm = T)
      ,DevTestConsumptionCostWithFullPrice = sum(DevTestConsumptionCostWithFullPrice, na.rm = T)
      ,CostSavingsFromDevTest = sum(CostSavingsFromDevTest, na.rm = T)
      ,CostSavingsFromRI = sum(CostSavingsFromRI, na.rm = T)
      ,CostSavingsTotal = sum(CostSavingsTotal, na.rm = T)
      ,CostUsageFromRI = sum(CostUsageFromRI, na.rm = T)
      ,FullPrice = mean(FullPrice, na.rm = T)
      ,RICost = sum(RICost, na.rm = T)
      ,RIHourRate = mean(RIHourRate, na.rm = T)
      ,RIHours = sum(RIHours, na.rm = T)
      ,RIHoursUsed = sum(RIHoursUsed, na.rm = T)
      ,RIRate = mean(RIRate, na.rm = T)
      ,UnitPrice = mean(UnitPrice, na.rm = T)
      ,ResourceRate= mean(ResourceRate, na.rm = T)
    )
  
    return (billingData2)
}