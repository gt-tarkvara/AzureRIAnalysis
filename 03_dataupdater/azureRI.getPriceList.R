
if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getPriceList <- function(apiObj, billingPeriod, ...) {
  
  temp_dir <- apiObj$cachedir
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = T)
  }
  
  query <- paste0("billingPeriods/", billingPeriod, "/pricesheet")
  
  filepath <- paste(temp_dir, paste0("pricesheet-", billingPeriod, ".json"), sep = .Platform$file.sep)
  
  #print(filepath)
  
  result <- azureRI.CallBillingApi(apiObj, version = "v2", query = query, filepath = filepath, reload = FALSE )
  
  if (is.na(result)) {
    return(tibble())
  }
  
  margin <- apiObj$margin
  
  result <- tryCatch(
    {
      fromJSON(result)
    },
    error = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    },
    warning = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    }
  ) %>%
    mutate(
      UnitPrice = unitPrice * margin
    ) %>%
    select(
      BillingPeriod = billingPeriodId,
      IncludedQuantity = includedQuantity,
      PartNumber = partNumber,
      UnitPrice,
      CurrencyCode = currencyCode
    )
  
  return(result)
}