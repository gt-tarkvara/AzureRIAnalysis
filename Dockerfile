# docker build -t herrbpl/azureri-etl -f Dockerfile .
FROM herrbpl/r-base-tidyverse:latest

# Temporary
RUN apt-get update -qq && apt-get -y --no-install-recommends install curl pandoc

## add docker user
RUN groupadd -g 1000 docker
RUN useradd -d /home/docker -g 1000 -u 1000 docker

COPY 03_dataupdater/azureRI.get* /home/docker/
COPY 03_dataupdater/azureRI.set* /home/docker/
COPY 03_dataupdater/azureRI.Call* /home/docker/
COPY 03_dataupdater/azureRI.R /home/docker/
COPY 03_dataupdater/run.*.R /home/docker/
COPY 03_dataupdater/UpdateData.Rmd /home/docker/
COPY 03_dataupdater/run.sh /home/docker/
