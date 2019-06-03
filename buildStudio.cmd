docker build -t rstudio-tidyverse -f Dockerfile.RStudio.intermediate .
docker tag rstudio-tidyverse:latest herrbpl/rstudio-tidyverse:latest
docker push herrbpl/rstudio-tidyverse:latest