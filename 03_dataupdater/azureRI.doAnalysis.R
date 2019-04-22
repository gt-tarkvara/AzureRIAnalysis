# Create analysis itself
if(!exists("azureRI", mode="function")) source("azureRI.R")

# TODO - cache dir location in environment variable

# get billing period
billingPeriod <- Sys.getenv("AZURERI_BILLINGPERIOD")

if (is.null(billingPeriod) || billingPeriod == "") {
  billingPeriod <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y%m")
}

message(paste0("Billing Period ", billingPeriod))

# download usage details 
usagedetails <- azureRI.getUsageDetails(billingPeriod = billingPeriod, tempdir = "/data/cache")

# Margin
margin <- Sys.getenv("AZURERI_MARGIN", unset = 1.0)
if (is.na(as.numeric(margin))) {
  warning(paste0("AZURERI_MARGIN ", margin, " is not a number!"),appendLF = T, immediate. = T )
  margin <- 1.0
} else {
  margin <- as.double(margin)
}

# Remove columns not needed ad adjusting cost with margin
usagedetails <- usagedetails %>% 
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

# load friendly service names. 
# TODO: make possible to load specific billing period (if saved to DB etc)
friendlyservicenames <- azureRI.getFriendlyServiceNames(filepath = "/data/cache/friendlyservicenames.xlsx" ) 

fsn <- friendlyservicenames %>%
  select(
    ConsumptionPartNumber,
    ReportedUnits,
    EnterpriseUnits,
    UnitOfMeasure,
    ConversionFactor
  ) 

usagedetails <- left_join(usagedetails, fsn, by = c("PartNumber" = "ConsumptionPartNumber")) %>%
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

  