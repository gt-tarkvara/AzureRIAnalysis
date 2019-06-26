# docker build -t herrbpl/azureri-etl -f Dockerfile .
FROM herrbpl/r-base-tidyverse:latest

## add docker user
RUN groupadd -g 1000 docker
RUN useradd -d /home/docker -g 1000 -u 1000 docker

COPY 03_dataupdater/azureRI.get* /home/docker/
COPY 03_dataupdater/azureRI.set* /home/docker/
COPY 03_dataupdater/azureRI.Call* /home/docker/
COPY 03_dataupdater/azureRI.R /home/docker/
COPY 03_dataupdater/UpdateData.Rmd /home/docker/