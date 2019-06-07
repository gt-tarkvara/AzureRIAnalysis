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
  ret <- NULL
  # if reload, then do not attempt to load from db/environment but rather reload from scratch
  if (reload) {
  
    # execute retrieving function
    ret <- do.call(fname, list(apiObj=apiObj, billingPeriod=billingPeriod, ... ))
    if (!is.null(ret) && length(ret) > 0 && is_empty(ret)) {
      
      assign(what, ret, pos = apiObj$env, envir = apiObj$env)
    }
    
    return(ret)
  } else {
    
    # if in environment cache
    if (exists(what, envir = apiObj$env)) {
      ret <- get(what, envir = apiObj$env)
    } else {
      
      # look in database
      
      # not found, 
      
      ret <- do.call(fname, list(apiObj=apiObj, billingPeriod=billingPeriod, ... ))
      
      assign(what, ret, pos = apiObj$env, envir = apiObj$env)
      
      
    }
    return(ret)
  }
  
  if (F) {
    
    # check if database connectivity exists
    # Not all function need to check database. So implementation should be left to specific function?
    
    # Now check if we have database connection available
    if (!is.null(apiObj$con) && dbIsValid(apiObj$con)) {
      
    }
  }
  
  
}