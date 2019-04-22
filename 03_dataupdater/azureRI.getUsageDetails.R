if(!exists("azureRI", mode="function")) source("azureRI.R")

azureRI.getUsageDetails <- function(obj = NULL, billingPeriod = NULL) {
  
  if (is.null(obj)) {
    obj <- azureRI.default
  }
  
  if (!is(obj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  # previous month if not specified
  if (is.null(billingPeriod)) {
    billingPeriod <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y%m")
  }
  
  # check if billingperiod is in acceptable limits
  periods <- azureRI.getBillingPeriods(obj)
  
  if (!(billingPeriod %in% periods[,1])) {
     warning("Invalid billing period specified", immediate. = TRUE)
     return(tibble())
  }
  
  
  q <- paste0("/usagedetails/download?billingPeriod=", billingPeriod)
  
  filepath <- paste(tempdir(), paste0("usagedetails-", billingPeriod, ".csv"), sep = .Platform$file.sep)
  
  print(filepath)
  
  result <- azureRI.CallBillingApi(obj, version = "v3", query = q, filepath = filepath, reload = FALSE )
  
  if (is.na(result)) {
    return(tibble())
  }
  
  result <- read_delim(file= filepath, ",", locale = locale(decimal_mark = ".", grouping_mark = ""), skip = 1 )
  
  jsonColumns <- ParseJSONColumn(result$AdditionalInfo)
  
  result <- cbind(result, jsonColumns)
  
  return(result)
}


PrepareForJSON <- function(x) {
  if (is.list(x) || is.vector(x)) {
    
    as.vector(sapply(s0, 
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