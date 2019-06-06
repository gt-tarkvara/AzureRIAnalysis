if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getUsageDetails <- function(apiObj, billingPeriod, ...) {
  
  # Helper functions start
  PrepareForJSON <- function(x) {
    if (is.list(x) || is.vector(x)) {
      
      as.vector(sapply(x, 
                       function(xx) {
                         if (xx == "{}") { "null" } 
                         else {
                           if (startsWith(xx, "{")) { xx } 
                           else { paste0("{ \"Misc\":\"", xx, "\"}")}
                         }
                       }, USE.NAMES = F))  
    } else {
      if (x == "{}") { "null" } 
      else {
        if (startsWith(x, "{")) { x } 
        else { paste0("{ \"Misc\":\"", x, "\"}")}
      }
    }
  }
  
  
  ParseJSONColumn <- function(x)  {
    
    str_c("[ ", str_c( PrepareForJSON( str_replace_na(x, replacement = "{}")), collapse = ",", sep=" "), " ]")  %>% 
      fromJSON(flatten = T) %>% 
      as_tibble()
  }
  
  # Helper functions end 
  
  temp_dir <- apiObj$cachedir
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = T)
  }
  
  q <- paste0("/usagedetails/download?billingPeriod=", billingPeriod)
  
  filepath <- paste(temp_dir, paste0("usagedetails-", billingPeriod, ".csv"), sep = .Platform$file.sep)
  
  #print(filepath)
  
  result <- azureRI.CallBillingApi(apiObj, version = "v3", query = q, filepath = filepath, reload = FALSE )
  
  if (is.na(result)) {
    return(tibble())
  }
  
  result <- read_delim(file= filepath, ",", locale = locale(decimal_mark = ".", grouping_mark = ""), skip = 1 )
  
  jsonColumns <- ParseJSONColumn(result$AdditionalInfo)
  
  margin <- apiObj$margin
  
  result <- as_tibble(cbind(result, jsonColumns)) %>%
    mutate(Cost = Cost*margin)
  
  # Apply calculations
  
  # Remove columns not needed ad adjusting cost with margin
  result <- result %>% 
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
  
  result <- left_join(result, azureRI.get("FriendlyServiceNames", apiObj = apiObj, billingPeriod = billingPeriod),
                            by = c("PartNumber" = "ConsumptionPartNumber")) %>%
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
    # TODO: Select only required variables
  
  return(result)
}

