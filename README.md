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
docker run  --env-file .\dev.env -it --rm -v /tmp/ricache:/data/cache  -w /home/docker -u docker -e AZURERI_RUNMODE=ALL_WITH_REPORT herrbpl/azureri-etl
```
dev.env in example is file containing environment variables

Mapping cache dir is optional but when mapped, location must be writable to user under which command is executed. By default, this is docker (uid:1000).


## To build a container

Layered approach is used
1. Base container (herrbpl/r-base-tidyverse) with tidyverse is built from rocker/r-ver image - this image build takes some time
2. Azure RI information ETL container (herrbpl/azureri-etl) is build based on herrbpl/r-base-tidyverse - just includes actual ETL scripts and some user-specific settings to run in docker context, rather than root

To build base container run buildRBaseImmediate.cmd or
```bash
docker build -t herrbpl/r-base-tidyverse -f Dockerfile.r-base.intermediate .
```
To build ETL container run
```bash
 docker build -t herrbpl/azureri-etl -f Dockerfile .
```

