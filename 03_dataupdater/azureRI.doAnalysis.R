# TODO: Refactor: azureRI.loadXXX - retrieves raw data from source, uses local file caching, parses raw data, adds billingperiod metadata
# TODO: Refactor: azureRI.getXXX - checks if cached in database and uses cache or loads and prepares data (joins, filter, selections) from azureRI.loadXXX function

# Create analysis itself
if(!exists("azureRI", mode="function")) source("azureRI.R")

# TODO - cache dir location in environment variable

# AzureRI Api object to use
apiObj <- azureRI.default

# get billing period
billingPeriod <- Sys.getenv("AZURERI_BILLINGPERIOD")

if (is.null(billingPeriod) || billingPeriod == "") {
  billingPeriod <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y%m")
}

message(paste0("Billing Period ", billingPeriod))

# Margin
margin <- Sys.getenv("AZURERI_MARGIN", unset = 1.0)
if (is.na(as.numeric(margin))) {
  warning(paste0("AZURERI_MARGIN ", margin, " is not a number!"),appendLF = T, immediate. = T )
  margin <- 1.0
} else {
  margin <- as.double(margin)
}

# === Friendly Service Names
# TODO: make possible to load specific billing period (if saved to DB etc)
friendlyServiceNames <- azureRI.getFriendlyServiceNames(filepath = "/data/cache/friendlyservicenames.xlsx" ) %>%
  select(
    Name,
    ConsumptionPartNumber,
    ReportedUnits,
    EnterpriseUnits,
    UnitOfMeasure,
    ConversionFactor,
    "Meter Category"
  ) %>%
  rename(MeterCategory = "Meter Category")

# === PriceSheet

priceSheet <- azureRI.getPriceList(obj = apiObj, billingPeriod = billingPeriod)



# === UsageDetails
# download usage details 
usageDetails <- azureRI.getUsageDetails(obj = apiObj, billingPeriod = billingPeriod)

# Remove columns not needed ad adjusting cost with margin
usageDetails <- usageDetails %>% 
  select(
    -AdditionalInfo,
    -ConsumedServiceId,
    -CostCenter,
    -DepartmentId,
    -DepartmentName,
    -ProductId,
    -ResourceLocation,
    -ResourceLocationId,
    -ServiceAdministratorId,
    -StoreServiceIdentifier,
    -SubscriptionId,
    -AccountId,
    -AccountOwnerEmail
  ) 

usageDetails <- left_join(usageDetails, friendlyServiceNames, by = c("PartNumber" = "ConsumptionPartNumber")) %>%
  mutate(
    ConversionFactor = as.numeric(ifelse(ConversionFactor == "Daily", 1/days_in_month(Date), ConversionFactor))
  ) %>%
  mutate(
    ConsumedUnits = ConsumedQuantity/ConversionFactor,
    EffectiveRate = ResourceRate*ConversionFactor,
    UnitOfMeasure = paste(formatC(EnterpriseUnits, digits=0,format="d"), ifelse(is.na(UnitOfMeasure.y), "",UnitOfMeasure.y) , " ")
  ) %>%
  select(
    -UnitOfMeasure.y,
    -UnitOfMeasure.x,
    -MeterCategory.y
  ) %>%
  rename(
    MeterCategory = MeterCategory.x
  )

# === ReservationCharge
# use default reservation charges
reservationCharge <- azureRI.getReservationCharges(obj = apiObj) 

# === InstanceSizeflexibility
instanceSizeFlexibility <- azureRI.getInstanceSizeFlexibility()

# === RIHoursWithRICosts
riHoursWithRICosts <- usageDetails %>%
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


riHoursWithRICosts <-left_join(x = riHoursWithRICosts, y = reservationCharge, 
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

riHoursWithRICosts <- left_join(x = riHoursWithRICosts, y = instanceSizeFlexibility, by = c("armSkuName"="Size")) %>%
  select(
    -billingperiod
  ) %>%
  rename(Maximum.Ratio = Ratio)

riHoursWithRICosts <- left_join(x = riHoursWithRICosts, y = instanceSizeFlexibility, by = c("RealArmSkuName"="Size")) %>%
  select(
    -billingperiod
  ) %>%
  rename(Actual.Ratio = Ratio) %>%
  mutate(
    usedRIRate = baseHourRate*(Actual.Ratio/Maximum.Ratio),
    RICost = ConsumedQuantity * usedRIRate
    ) %>%
  group_by(InstanceId, Date, SubscriptionGuid, ConsumptionMeter) %>%
  summarise(
    RIHours=sum(ConsumedQuantity, na.rm = T), 
    RIRate=mean(usedRIRate,na.rm = T), 
    RICost=sum(RICost, na.rm = T)
  )


# === DevTestMapping

devTestMapping <- friendlyServiceNames %>%
    filter(MeterCategory == "Virtual Machines")

devTestMapping <- left_join(x = devTestMapping, y = priceSheet, by = c("ConsumptionPartNumber" = "partNumber"))

devTestMapping_B <- devTestMapping %>%
    mutate(NameWithoutDevTest = str_replace(Name, "Dev/Test - ","")) %>%
    filter(str_detect(Name, "Dev/Test") ) %>%
    select(
      NameWithoutDevTest,
      DevTestPrice = unitPrice,
      DevTestPartNumber = ConsumptionPartNumber
    )

devTestMapping <- left_join(x=devTestMapping_B, y=devTestMapping , by=c("NameWithoutDevTest" = "Name")) %>%
  select(
    Name = NameWithoutDevTest,
    FullPricePartNumber = ConsumptionPartNumber,
    FullPrice = unitPrice,
    DevTestPartNumber,
    DevTestPrice
  )

rm(devTestMapping_B)


# === UsageDetailsWithEmptyRows
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
  group_by(SubscriptionGuid, Date, InstanceId) %>%
  count() %>%
  select(
    -n
  )

instanceNames <- left_join(x=instanceNames, y=vmDetails, by=c("SubscriptionGuid"="SubscriptionGuid", "InstanceId"="InstanceId"))

usageDetailsWithEmptyRows <- bind_rows(usageDetails, instanceNames) %>%
  rename(ExtendedCost = Cost)

rm(instanceNames)
rm(vmDetails)


# === BillingData
billingData <- usageDetailsWithEmptyRows %>%
  group_by(Date, InstanceId, MeterId, SubscriptionName, Product, MeterSubCategory, MeterCategory, UnitOfMeasure, PartNumber,
           ConversionFactor
           ) %>%
  summarise(
    ConsumedUnits = sum(ConsumedUnits, na.rm = T),
    EffectiveRate = mean(EffectiveRate, na.rm = T),
    ExtendedCost = sum(ExtendedCost, na.rm =T )
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


