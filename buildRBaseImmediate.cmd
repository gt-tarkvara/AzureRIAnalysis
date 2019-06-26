docker build -t herrbpl/r-base-tidyverse -f Dockerfile.r-base.intermediate .
rem docker tag r-base-tidyverse:latest herrbpl/r-base-tidyverse:latest
docker push herrbpl/r-base-tidyverse:latest