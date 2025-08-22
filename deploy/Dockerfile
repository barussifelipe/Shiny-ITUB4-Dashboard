# Use the official R image with shiny support
FROM rocker/shiny-verse:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('DBI', 'shinydashboard', 'dplyr', 'shinyWidgets', 'ggplot2', 'tidyr', 'DT', 'sqldf', 'RPostgres'))"

# Copy app files
COPY . /srv/shiny-server/

# Make all app files readable (required for Shiny Server)
RUN chmod -R 755 /srv/shiny-server/

# Expose port
EXPOSE 3838

# Command to run the app
CMD ["/usr/bin/shiny-server"]