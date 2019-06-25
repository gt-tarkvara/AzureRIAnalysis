#' AzureRI info get function
#'
#' This function tries to retrieve information about your Azure RI instances for billing period
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

azureRI.get <- function(what, apiObj=NULL, billingPeriod = NULL, reload = FALSE, row.names = "BusinessKey", ...) {

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
  names(periods)
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
  
  varname <- paste0(what, billingPeriod)
  
  # if reload, then do not attempt to load from db/environment but rather reload from scratch
  if (reload) {
  
    # execute retrieving function
    ret <- do.call(fname, list(apiObj=apiObj, billingPeriod=billingPeriod, ... ))
    if (!is.null(ret) && length(ret) > 0 && is_empty(ret)) {
      
      assign(varname, ret, pos = apiObj$env, envir = apiObj$env)
    }
    
    return(ret)
  } else {
    
    
    
    # if in environment cache
    if (exists(varname, envir = apiObj$env)) {
      ret <- get(varname, envir = apiObj$env)
    } else {
      
      # look in database
      if (!is.null(apiObj$con) && dbIsValid(apiObj$con) && dbExistsTable(apiObj$con, dbQuoteIdentifier(apiObj$con,c(what)) )) {
         
         sql <- paste0("select * from ", dbQuoteIdentifier(apiObj$con,c(what)), " where BillingPeriod = ", 
                              dbQuoteString(apiObj$con, billingPeriod))
         warning(paste0("Executing ", sql))
         ret <- dbGetQuery(apiObj$con, sql) %>% as.tibble() %>% select(-!!row.names) 
         if (nrow(ret) == 0) {
           ret <- do.call(fname, list(apiObj=apiObj, billingPeriod=billingPeriod, ... ))
         }
         
      } else {
      
        # not found, 
      
        ret <- do.call(fname, list(apiObj=apiObj, billingPeriod=billingPeriod, ... ))
      }
      
      assign(varname, ret, pos = apiObj$env, envir = apiObj$env)
      
      
    }
    return(ret)
  }
}