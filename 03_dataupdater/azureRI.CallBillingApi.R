azureRI.CallBillingApi <- function(obj, version = NULL, query=NULL, filepath=NULL, reload=FALSE, handler=NULL) {
  
  if (!is(obj, "azureRI")) {
    stop("Expected AzureRI object")
  }
  
  if (is.null(version)) {
    version <- "v3"
  }
  if (is.null(query)) {
    query <- "billingperiods"
  }
  
  enrollmentNumber <- obj$enrollmentNumber
  
  iurl <- "https://consumption.azure.com/${version}/enrollments/${enrollmentNumber}/${query}"
  furl <- qq(iurl, code.pattern = "\\$\\{CODE\\}")
  message(paste0("Retrieving ", furl))
  h <- curl::new_handle()
  curl::handle_setopt(h, http_version = 0L)
  curl::handle_setheaders(h, "Authorization" = paste("Bearer", obj$bearer, sep = " "))
  
  if (!is.null(filepath)) {
    
    if (file.exists(filepath) && reload) {
      file.remove(filepath)
    }
    
    if (!file.exists(filepath)) {
      
      req <- tryCatch(
        { 
          curl::curl_download(furl, filepath, mode = "wb+", handle = h, ) 
        },
        error=function(cond) {
          errtxt <- paste0("Got status message ", toString(req$status_code), " while attepting to download. Have you set AZURERI_ENROLLMENTNO and AZURERI_BEARER correctly?")
          warning(errtxt,immediate. = TRUE)
          unlink(filepath)
          return(NA)
        },
        warning=function(cond) {
          errtxt <- paste0("Got status message ", toString(req$status_code), " while attepting to download. Have you set AZURERI_ENROLLMENTNO and AZURERI_BEARER correctly?")
          warning(errtxt,immediate. = TRUE)
          unlink(filepath)
          return(NA)
        }
      )
      if (is.na(req)) {
        return(NA)
      }
      rm(req)
    }
    return(filepath)
    
  } else {
    
    if (!is.null(handler) && is.function(handler)) {
      con <- curl(url=furl, handle = h)
      response <- handler(con)
      if(isOpen(con)) {
        close(con)
      }
    } else {
      
      req <- curl_fetch_memory(url= furl, handle = h)
      
      
      if (strsplit(toString(req$status_code),"")[[1]][1] == "4") {
        
        errtxt <- paste0("Got status message ", toString(req$status_code), " while attepting to download. Have you set AZURERI_ENROLLMENTNO and AZURERI_BEARER correctly?")
        warning(errtxt,immediate. = TRUE)
        return(NA)
      }
      
      if (strsplit(toString(req$status_code),"")[[1]][1] != "2") {
        errtxt <- paste0("Got status message ", toString(req$status_code), " while attepting to download. Have you set AZURERI_ENROLLMENTNO and AZURERI_BEARER correctly?")
        warning(errtxt,immediate. = TRUE)
        return(NA)
      }
      
      response <- rawToChar(req$content)
      
      rm(req)
    }
    return(response)
  }
  # Read file into dataframe?
  
  #return(read_file(filename))
  
}