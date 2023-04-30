# Use the official R base image
FROM rocker/r-base:latest

# Update the system and install required libraries
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    pandoc \
    libcairo2-dev \
    libxt-dev \
    git-core \
    wget

# Install necessary R packages
RUN R -e "install.packages(c('knitr', 'rmarkdown', 'dplyr', 'tidyr', 'timetk', 'quantmod', 'Quandl', 'ggplot2', 'apexcharter', 'DT', 'rvest', 'htmltools', 'jsonlite', 'scales', 'xts', 'roll', 'TTR', 'here', 'magrittr', 'tidyquant'))"

# Set the working directory
WORKDIR /app

# Copy R markdown file and any other required files to the working directory
COPY precious_metals.Rmd /app/precious_metals.Rmd
COPY quandl_api.json /app/quandl_api.json
COPY src /app/src
COPY data /app/data

EXPOSE 3838
# Run the R markdown file to generate the output HTML document
CMD ["R", "-e", "rmarkdown::run('/app/precious_metals.Rmd', shiny_args = list(host = '0.0.0.0', port = 3838))"]
