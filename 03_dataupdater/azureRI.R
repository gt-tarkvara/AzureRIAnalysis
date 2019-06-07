library(GetoptLong)
library(curl)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)

# TODO: make this object reference object so its reference can be passed around

azureRI <- function(enrollmentno, bearer, margin=1.0, cachedir = tempdir(), con = NULL) {
  
  if (is.na(as.numeric(margin))) {
    warning(paste0("Margin ", margin, " is not a number!"),appendLF = T, immediate. = T )
    margin <- 1.0
  } else {
    margin <- as.double(margin)
  }
  
  value <- list(
    enrollmentNumber = enrollmentno, 
    bearer = bearer, 
    margin=as.numeric(margin), 
    cachedir = cachedir, 
    env = new.env(parent = emptyenv()),
    sqlCache = list(
      "BillingData" = c("Date", "SubscriptionName", "Product", "PartNumber", "InstanceId"),
      "RIHoursUtilization" = c("Date")
    )
    
    )
  attr(value, "class") <- "azureRI"
  
  # register db cacheable classes, along with key definitions
  
  
  value
}

if(!exists("azureRI.get", mode="function")) source("./azureRI.get.R")
if(!exists("azureRI.getMargin", mode="function")) source("./azureRI.getMargin.R")

if(!exists("azureRI.CallBillingApi", mode="function")) source("./azureRI.CallBillingApi.R")
if(!exists("azureRI.getInstanceSizeFlexibility", mode="function")) source("./azureRI.getInstanceSizeFlexibility.R")
if(!exists("azureRI.getFriendlyServiceNames", mode="function")) source("./azureRI.getFriendlyServiceNames.R")
if(!exists("azureRI.getBillingPeriods", mode="function")) source("./azureRI.getBillingPeriods.R")
if(!exists("azureRI.getReservationCharges", mode="function")) source("./azureRI.getReservationCharges.R")
if(!exists("azureRI.getPriceList", mode="function")) source("./azureRI.getPriceList.R")
if(!exists("azureRI.getUsageDetails", mode="function")) source("./azureRI.getUsageDetails.R")
if(!exists("azureRI.getBillingData", mode="function")) source("./azureRI.getBillingData.R")
if(!exists("azureRI.getRIHoursUtilization", mode="function")) source("./azureRI.getRIHoursUtilization.R")
if(!exists("azureRI.getRIHoursWithRICosts_raw", mode="function")) source("./azureRI.getRIHoursWithRICosts_raw.R")
if(!exists("azureRI.getRIHoursWithRICosts", mode="function")) source("./azureRI.getRIHoursWithRICosts.R")
if(!exists("azureRI.getDevTestMapping", mode="function")) source("./azureRI.getDevTestMapping.R")

# default azureRI object
azureRI.default <- azureRI(Sys.getenv("AZURERI_ENROLLMENTNO"), Sys.getenv("AZURERI_BEARER"), Sys.getenv("AZURERI_MARGIN"), Sys.getenv("AZURERI_CACHEDIR"))
#azureRI.default$billingPeriods <- azureRI.getBillingPeriods()



#xyz <- azureRI.getBillingPeriods(obj = azureRI.default)

