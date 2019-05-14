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
    ConsumptionPartNumber,
    ReportedUnits,
    EnterpriseUnits,
    UnitOfMeasure,
    ConversionFactor
  ) 




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
  ) %>%
  mutate(Cost = Cost*margin)



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
  )

# === ReservationCharge
# use default reservation charges
reservationCharge <- azureRI.getReservationCharges(obj = apiObj) %>%
  mutate(
    baseHourRate = baseHourRate * margin,
    amount = amount * margin
  )

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
  summarise(RIHours=sum(ConsumedQuantity), RIRate=mean(usedRIRate), RICost=sum(RICost))

# === PriceSheet


