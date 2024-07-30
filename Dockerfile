# start from the rstudio/plumber image
FROM rocker/r-ver:4.2.3

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libsodium-dev
    
# Install R packages
RUN R -e "install.packages(c('plumber', 'caret', 'dplyr', 'tibble', 'readxl', 'tidyr', 'rpart', 'randomForest', 'rpart.plot', 'doParallel', 'e1071'), repos='https://cran.rstudio.com/')"

# Copy the R script and dataset into the Docker image
COPY api.R api.R
COPY diabetes_binary_health_indicators_BRFSS2015.xlsm diabetes_binary_health_indicators_BRFSS2015.xlsm

# Expose port 8000 for the API
EXPOSE 8000

# Run the API script when the container starts
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('api.R'); pr$run(host = '0.0.0.0', port = 8000)"]

