#' AzureRI info get function
#'
#' This function tries to retrieve information about your Azure RI instances for billing period
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

azureRI.get <- function(what, apiObj=NULL, billingPeriod = NULL, reload = FALSE, row.names = "BusinessKey", ...) {

  # Should we cache things for apiObj?  
  # Optimize later.. 
  
  # Input check
  if (is.null(apiObj)) {
    apiObj <- azureRI.default
  }
  
  if (!is(apiObj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  # previous month if not specified
  if (is.null(billingPeriod)) {
    billingPeriod <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y%m")
  }
  
  
  
  # check if billingperiod is in acceptable limits
  periods <- azureRI.getBillingPeriods(apiObj)
  
  if (!(billingPeriod %in% periods$billingPeriodId)) {
    warning(paste0("Invalid billing period specified - ", billingPeriod) , immediate. = TRUE)
    return(tibble())
  }
  
  
  fname <- NA
  if (is.character(what)) {
    fname <- paste0("azureRI.get",what)
  } else {
    # TODO: list available options
    stop("Expected either a character as what")
  }
  #if (exists(fname, where='package:azureRI' mode='function'))
  #cat(fname)
  
  if (!exists(fname, mode="function")) {
     # TODO: list available options
     stop(paste0("Function ", fname," not found"))
  }
  
  #cat(paste(list(...)))
  
  # execute retrieving function
  ret <- do.call(fname, list(apiObj=apiObj, billingPeriod=billingPeriod, ... ))
  return(ret)
  
  if (F) {
    
    # check if database connectivity exists
    # Not all function need to check database. So implementation should be left to specific function?
    
    # Now check if we have database connection available
    if (!is.null(apiObj$con) && dbIsValid(apiObj$con)) {
      
    }
  }
  
  
}