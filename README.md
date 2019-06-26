# AzureRIAnalysis

These are bunch of R scripts to download Azure Reserved Instances usage data along with pricelist andother auxiliary information.

Scripts then calculate cost savings from RI and Dev/Test usage and stores updated data to database.

**Warning** - As Microsoft changes data structures from time to time, these scripts might break or start to give strange results. No warranty given.
## Environment variables

Required for R
```bash
AZURERI_ENROLLMENTNO=<Azure Enrollment Number, mandatory>
AZURERI_BEARER=<Azure Billing API token, mandatory>
AZURERI_MARGIN=<Partner margin, for example 1.0, if ommited, 1.0>
AZURERI_BILLINGPERIOD=<Billing Period, for example "201901", if omitted, last month>
AZURERI_CACHEDIR=<cache location, for example /data/cache>
AZURERI_CONNECTIONSTRING=<Database connection string, for example Azure SQL database connection string>
```
Required for bash script to send report
```bash
AZURERI_STORAGEACCOUNT=<Azure storage account name, optional>
AZURERI_STORAGECONTAINER=<Azure storage account container name, optional>
AZURERI_SASTOKEN=<Azure storage account SAS token, optional>
AZURERI_RUNMODE=[ALL | AUX | USAGE | ALL_WITH_REPORT]
```
## To run in R

```R
source('azureRI.R')
billingData <- azureRI.get("BillingData", billingPeriod = "201905", reload = T)
head(billingData)
```

## To run container

```bash
docker run  --env-file .\dev.env -it --rm -v /tmp/ricache:/data/cache  -w /home/docker -u docker  herrbpl/azureri-etl
```
dev.env in example is file containing environment variables