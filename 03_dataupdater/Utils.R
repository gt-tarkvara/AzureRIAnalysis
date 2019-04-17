util.download <- function(url, filepath, headers, reload=FALSE) {
  
  if (file.exists(filepath) && reload) {
    file.remove(filepath)
  }
  
  if (!file.exists(filepath)) {
    h <- curl::new_handle()
    curl::handle_setopt(h, http_version = 0L)
    #curl::handle_setheaders(h, "Authorization" = paste("Bearer", bearer, sep = " "))
    if (!is.null(headers) && headers != "") {
      curl::handle_setheaders(h, headers)
    }
    req <- curl::curl_download(furl, filename, mode = "wb+", handle = h)
  }
  
  return(filepath)
}
