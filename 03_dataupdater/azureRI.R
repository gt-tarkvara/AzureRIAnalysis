library(GetoptLong)
library(curl)
library(tidyverse)
library(jsonlite)
library(rvest)

azureRI <- function(enrollmentno, bearer) {
  value <- list(enrollmentNumber = enrollmentno, bearer = bearer)
  attr(value, "class") <- "azureRI"
  value
}

source("./azureRI.CallBillingApi.R")
source("./azureRI.getInstanceSizeFlexibility.R")
source("./azureRI.getFriendlyServiceNames.R")
source("./azureRI.getBillingPeriods.R")
source("./azureRI.getReservationCharges.R")
source("./azureRI.getUsageDetails.R")

# default azureRI object
azureRI.default <- azureRI(Sys.getenv("AZURERI_ENROLLMENTNO"), Sys.getenv("AZURERI_BEARER"))
azureRI.default$billingPeriods <- azureRI.getBillingPeriods(obj = azureRI.default)
#xyz <- azureRI.getBillingPeriods(obj = azureRI.default)

