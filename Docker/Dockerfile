# get shiny server and R from the rocker project
FROM rocker/shiny:4.0.5

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinyFiles", \
              "GGIR", \
              "bslib", \
              "methods", \
              "jsonlite", \
              "DT", \
              "waiter", \
              "activityCounts" \
              "remotes" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'
RUN R -e 'remotes::install_github("habitus-eu/HabitusGUI")' 

RUN mkdir -p /code

# run app by running the script
COPY /code/app.R /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]
