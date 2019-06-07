
if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getReservationCharges <- function(apiObj = NULL, startdate = Sys.Date() - 3*365, enddate = Sys.Date(), ...) {
  
  
  temp_dir <- apiObj$cachedir
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = T)
  }
  
  query <- paste0("reservationcharges?startDate=", startdate ,"&endDate=", enddate)
  
  filepath <- paste(temp_dir, paste0("reservationCharges-", startdate, "-", enddate, ".json"), sep = .Platform$file.sep)
  
  #print(filepath)
  
  result <- azureRI.CallBillingApi(apiObj, version = "v3", query = query, filepath = filepath, reload = FALSE )
  
  
  if (is.na(result)) {
    return(tibble())
  }
  
  
  #str(result)
  margin <- apiObj$margin
  
  result <- tryCatch(
    {
      as.tibble(fromJSON(result))
    },
    error = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    },
    warning = function(cond) {
      warning(cond, immediate. = TRUE)
      return(tibble())
    }
  ) 
  
  if (length(result) == 0) {
    # Should return empty frame
    return(NA)
  }
  result <- result %>%
    mutate(
      baseHourRate = if_else(term=="P1Y",(amount/(365*24))/quantity, (amount/(3*365*24))/quantity)
    ) %>%
    mutate(
      baseHourRate = baseHourRate * margin,
      amount = amount * margin
    ) %>%
    filter(
      eventType == "Purchase"
    ) %>% 
    select( #Standardizing output
      PurchasingEnrollment=purchasingEnrollment,
      ArmSkuName=armSkuName,
      Term=term,
      Region=region,
      PurchasingSubscriptionGuid=purchasingSubscriptionGuid,
      PurchasingSubscriptionName=purchasingSubscriptionName,
      AccountName=accountName,
      AccountOwnerEmail=accountOwnerEmail,
      DepartmentName=departmentName,
      CostCenter=costCenter,
      CurrentEnrollment=currentEnrollment,
      EventDate=eventDate,
      ReservationOrderId=reservationOrderId,
      Description=description,
      EventType=eventType,
      Quantity=quantity,
      Amount=amount,
      Currency=currency,
      ReservationOrderName=reservationOrderName,
      BaseHourRate=baseHourRate,
      Amount=amount
    )
  
  return(result)
}