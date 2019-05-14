library(GetoptLong)
library(curl)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)

azureRI <- function(enrollmentno, bearer, margin=1.0, cachedir = tempdir()) {
  value <- list(enrollmentNumber = enrollmentno, bearer = bearer, margin=margin, cachedir = cachedir)
  attr(value, "class") <- "azureRI"
  value
}

if(!exists("azureRI.CallBillingApi", mode="function")) source("./azureRI.CallBillingApi.R")
if(!exists("azureRI.getInstanceSizeFlexibility", mode="function")) source("./azureRI.getInstanceSizeFlexibility.R")
if(!exists("azureRI.getFriendlyServiceNames", mode="function")) source("./azureRI.getFriendlyServiceNames.R")
if(!exists("azureRI.getBillingPeriods", mode="function")) source("./azureRI.getBillingPeriods.R")
if(!exists("azureRI.getReservationCharges", mode="function")) source("./azureRI.getReservationCharges.R")
if(!exists("azureRI.getPriceList", mode="function")) source("./azureRI.getPriceList.R")
if(!exists("azureRI.getUsageDetails", mode="function")) source("./azureRI.getUsageDetails.R")

# default azureRI object
azureRI.default <- azureRI(Sys.getenv("AZURERI_ENROLLMENTNO"), Sys.getenv("AZURERI_BEARER"), Sys.getenv("AZURERI_MARGIN"), Sys.getenv("AZURERI_CACHEDIR"))
azureRI.default$billingPeriods <- azureRI.getBillingPeriods()
#xyz <- azureRI.getBillingPeriods(obj = azureRI.default)

