# SQL save test
library(DBI)
#install.packages("odbc")
library(odbc)

connstr <- Sys.getenv("AZURERI_CONNECTIONSTRING")

con <- DBI::dbConnect(
  odbc()
  , Driver = "{ODBC Driver 17 for SQL Server}"
  , .connection_string = connstr)



system.time(dbWriteTable(con, "test1", as.data.frame(billingData), overwrite=T))





