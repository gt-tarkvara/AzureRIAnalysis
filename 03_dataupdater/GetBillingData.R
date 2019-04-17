library(GetoptLong)
library(httr)
library(readr)
library(jsonlite)

CallBillingApi <- function(cachedir, filename, enrollmentno, version, bearer, query, reload=FALSE) {
	if (is.null(version)) {
		version <- "v3"
	}
	if (is.null(query)) {
		query <- "billingperiods"
	}

	iurl <- "https://consumption.azure.com/${version}/enrollments/${enrollmentNumber}/${query}"
	furl <- qq(iurl, code.pattern = "\\$\\{CODE\\}")
  print(furl)
	
	if (!dir.exists(cachedir)) {
	   if (!dir.create(cachedir)) {
	       stop(paste("Unable to create cachedir", cachedir))
	   }
	}
	
	filename <- basename(filename)
	filepath <- paste(cachedir, filename, "/")
	
	if (file.exists(filepath) && reload) {
	   file.remove(filepath)
	}
	
	if (!file.exists(filepath)) {
  	h <- curl::new_handle()
  	curl::handle_setopt(h, http_version = 0L)
  	curl::handle_setheaders(h, "Authorization" = paste("Bearer", bearer, sep = " "))
  	req <- curl::curl_download(furl, filename, mode = "wb+", handle = h)
	}
	
	# Read file into dataframe?
	
	return(read_file(filename))
	
}

bearer <- 'eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsIng1dCI6ImpoeXA2UU9DWlZmY1pmdmhDVGR1OFdxeTJ5byJ9.eyJFbnJvbGxtZW50TnVtYmVyIjoiNzk1MzUxMTAiLCJJZCI6IjA0MTUzOTI0LTJmYzYtNDhhOC1hMmNlLTM3YzY0Y2ZmNTYyYSIsIlJlcG9ydFZpZXciOiJQYXJ0bmVyIiwiUGFydG5lcklkIjoiMTU1NzYiLCJEZXBhcnRtZW50SWQiOiIiLCJBY2NvdW50SWQiOiIiLCJpc3MiOiJlYS5taWNyb3NvZnRhenVyZS5jb20iLCJhdWQiOiJjbGllbnQuZWEubWljcm9zb2Z0YXp1cmUuY29tIiwiZXhwIjoxNTcwNTQwNDkyLCJuYmYiOjE1NTQ3MjkyOTJ9.N8otirflzW9jQ6F5TuQwSE8a7VEg1yr-99tLfJF_8PytTB0NehlvRqsCfKmo7QTdR1UBXSU8eLEOQARRbOHrSKdBef6PLgyOETcyUtNx1wC2F26qKjP6rf7VZPqysN3ebMBa6GNkcrT7epM6cvr2EeCbayZ6qyd3VsrnvepZBcVJojmA5hXVo6mqtyDdTYQo2FvFYqd4Vrn7gUmf0vqBNcKhXyVQo8QB8Vf1STTutWKv52tW6AONLlm1BPpsESVVcCQAHEs9Y-Os33T8d4jatxrz0ZJtWR_TqtDBk5Lf1LAbBe0dgU2CTM7JxZ13p6TlbdjdYvbuEK5_dGpGAVNIqg'
         
enrollmentNumber <- '79535110'

testf <- CallBillingApi("/tmp", "billinperiods.json", enrollmentNumber, "v2", bearer, NULL)