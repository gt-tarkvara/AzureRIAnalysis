# SQL save test
library(DBI)
#install.packages("odbc")
library(odbc)

connstr <- Sys.getenv("AZURERI_CONNECTIONSTRING")

con <- DBI::dbConnect(
  odbc()
  , Driver = "{ODBC Driver 17 for SQL Server}"
  , .connection_string = connstr)



#system.time(dbWriteTable(con, "test1", as.data.frame(billingData), overwrite=T))



# test update for specific rows.. 
# i have no business key for billing data row..
# billingPeriod, enrollmentNumber, subscriptionGuid, Date, Product, InstanceId, meterId, PartNumber
# need to remove all old values and replace with new ones.
# should be done on server
# upload to temporary table
# use sql statement to move data from temp table to data
# need to standardize field names

library(digest)

# calculate hash
a <- billingData %>% mutate( hash = digest(c(Date, SubscriptionName, Product, PartNumber, InstanceId ), algo="md5"))
b <- column_to_rownames(a, var="hash")

# system.time(dbWriteTable(con, "test4", as.data.frame(b), overwrite=T, row.names=T))

#list tables

# test partial update

aa <- mtcars

aa$billingPeriod = '201812'

ab <- mtcars
ab$billingPeriod = '201901'

taa <- as.tibble(rownames_to_column(aa))
tab <- as.tibble(rownames_to_column(ab))

# Apply hash
bb <- rbind(taa, tab) %>% rowwise() %>%
  mutate(BusinessKey = digest(c(rowname, billingPeriod ))) %>%
  column_to_rownames(var="BusinessKey")

#create table

# Check if table exists
dbExistsTable(con, "mtcarsx")

#Add table with primary key "BusinessKey"
dbWriteTable(con, "mtcarsx", as.data.frame(bb)[0,], append=T, row.names="BusinessKey" )
#dbBegin(con)
rs <- dbExecute(con, "alter table mtcarsx alter column BusinessKey varchar(255) not null")
rs <- dbExecute(con, "alter table mtcarsx add constraint pk_mtcarsx primary key clustered (BusinessKey)")
#dbCommit(con)
dbWriteTable(con, "mtcarsx", as.data.frame(bb), append=T, row.names="BusinessKey" )

# delete some rows
rs <- dbExecute(con, "delete from mtcarsx where billingPeriod='201901'")

#reinsert subset
dd <- bb[bb$billingPeriod == "201901",]
dbWriteTable(con, "mtcarsx", as.data.frame(dd), append=T, row.names="BusinessKey")

# add primary key

rs <- dbExecute(con, "alter table mtcarsx add constraint pk_mtcarsx primary_key clustered (row_names, billingPeriod)")


