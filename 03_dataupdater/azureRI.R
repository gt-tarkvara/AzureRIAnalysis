library(GetoptLong)
library(curl)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)

azureRI <- function(enrollmentno, bearer, margin=1.0) {
  value <- list(enrollmentNumber = enrollmentno, bearer = bearer, margin=margin)
  attr(value, "class") <- "azureRI"
  value
}

source("./azureRI.CallBillingApi.R")
source("./azureRI.getInstanceSizeFlexibility.R")
source("./azureRI.getFriendlyServiceNames.R")
source("./azureRI.getBillingPeriods.R")
source("./azureRI.getReservationCharges.R")
source("./azureRI.getPriceList.R")
source("./azureRI.getUsageDetails.R")

# default azureRI object
azureRI.default <- azureRI(Sys.getenv("AZURERI_ENROLLMENTNO"), Sys.getenv("AZURERI_BEARER"), Sys.getenv("AZURERI_MARGIN"))
azureRI.default$billingPeriods <- azureRI.getBillingPeriods()
#xyz <- azureRI.getBillingPeriods(obj = azureRI.default)

