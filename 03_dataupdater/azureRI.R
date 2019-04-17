library(GetoptLong)

azureRI <- function(enrollmentno, bearer) {
  value <- list(enrollmentNumber = enrollmentno, bearer = bearer)
  attr(value, "class") <- "azureRI"
  value
}


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
  
  h <- curl::new_handle()
  curl::handle_setopt(h, http_version = 0L)
  curl::handle_setheaders(h, "Authorization" = paste("Bearer", obj$bearer, sep = " "))
  
  
  if (!is.null(filepath)) {
  
    if (file.exists(filepath) && reload) {
      file.remove(filepath)
    }
    
    if (!file.exists(filepath)) {
     
      req <- curl::curl_download(furl, filename, mode = "wb+", handle = h)
    }
    return(filepath)
  
  } else {
    
    if (!is.null(handler) && is.function(handler)) {
       con <- curl(url=furl, handle = h)
       response <- handler(con)
       #close(con)
    } else {
      
      req <- curl_fetch_memory(url= furl, handle = h)
      response <- rawToChar(req$content)
      
    }
    return(response)
  }
  # Read file into dataframe?
  
  #return(read_file(filename))
  
}


getx <- function(con) {
  open(con, "rb", blocking=FALSE)
  data <- ""
  
  while(isIncomplete(con)){
    buf <- readBin(con, raw(), 1024)
    if(length(buf)) 
      data <- paste0(data, rawToChar(buf))
  }
  
  #while(length(x <- readLines(con, n = 5))){
  #  data <- paste(data, x, sep="")
  #}
  close(con)
  return(fromJSON(data))
}