#' AzureRI info set function
#'
#' This function tries to store information about your Azure RI instances for billing period to configured database
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

azureRI.set <- function(what, data, apiObj, billingPeriod, reload = FALSE, row.names = "BusinessKey", ...) {
  # Input check
  if (is.null(apiObj)) {
    apiObj <- azureRI.default
  }
  
  if (!is(apiObj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  # check if what is in save list
  if ( !(what %in% names(apiObj$sqlCache))) {
    stop(paste0(what , " is not in sql Caching list"))
    return(F)
  }
  
  
  # previous month if not specified
  if (is.null(billingPeriod)) {
    stop("billingPeriod parameter is mandatory")
    return(F)
  }
  
  # check if billingperiod is in acceptable limits
  periods <- azureRI.getBillingPeriods(apiObj)
  
  if (!(billingPeriod %in% periods$billingPeriodId)) {
    warning(paste0("Invalid billing period specified - ", billingPeriod) , immediate. = TRUE)
    return(F)
  }
  
  if (is.null(data)) { 
     return(T)
  }
  
  if (!is.data.frame(data) && !is.tibble(data)) {
    stop("Only data.frame or tibble are supported as data input") 
  }
  
  if (length(data) == 0) {
    return(T)
  }
  
  
  # use specified billing period for data manipulation. While data itself can contain billing period, it can vary. So we use one specified
  data$BillingPeriod = billingPeriod
  
  # create Key for database operation (apply md5 hash)
  fi <- apiObj$sqlCache[[what]]
  
  fn <- parse_expr(paste("digest( c(.data$", paste(fi, sep=",", collapse = ", .data$"), "), algo='md5')",sep=""))
  
  varname <- row.names
  
  data <- data %>% rowwise() %>%
    mutate(!!varname := !!fn) %>%
    column_to_rownames(var=row.names)
  
  # Update database
  # look in database
  if (!is.null(apiObj$con) && dbIsValid(apiObj$con)) { 
    
    # Create new table if required
    if (!dbExistsTable(apiObj$con, what)) {
      warning(paste0("creating table '", what, "'"), immediate. = TRUE)
      dbWriteTable(apiObj$con, dbQuoteIdentifier(apiObj$con,c(what)), as.data.frame(data)[0,], append=T, row.names= row.names )
      #dbBegin(apiObj$con)
      
      sql <- paste0("alter table ", 
                    dbQuoteIdentifier(apiObj$con, c(what)), " alter column ",
                    dbQuoteIdentifier(apiObj$con,c(row.names))," varchar(255) not null")
      
      warning(paste0("Executing ", sql))
      rs <- dbExecute(apiObj$con, sql)
      sql <- paste0("alter table ", dbQuoteIdentifier(apiObj$con,c(what)), " add constraint ", 
                    dbQuoteIdentifier(apiObj$con,c(paste0("pk_",what)))," primary key clustered (",
                    dbQuoteIdentifier(apiObj$con, row.names),")")
      warning(paste0("Executing ", sql))
      rs <- dbExecute(apiObj$con, sql)
      #dbCommit(apiObj$con)
    }
    
    # delete billing period data
    sql <- paste0("delete from ", 
                  dbQuoteIdentifier(apiObj$con,what), " where billingPeriod=",
                  dbQuoteString(apiObj$con,billingPeriod))
    warning(paste0("Executing ", sql))
    rs <- dbExecute(apiObj$con, sql)
    
    #insert data
    dbWriteTable(apiObj$con, dbQuoteIdentifier(apiObj$con,what), as.data.frame(data), append=T, row.names= row.names )
    
  } else {
    warning("Database connection not open, cannot save to database", immediate. = TRUE)
    return (F)
  }
  
  return(T)
   
}