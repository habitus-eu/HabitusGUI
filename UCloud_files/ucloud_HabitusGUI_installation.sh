# Python / PALMSpy dependencies
conda create -n palmspy python=3.7 openjdk=8.0 make=4.2.1 -y
conda activate palmspy
make
make install
make clean

# R / HabitusGUI dependencies
sudo apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev

R -e 'install.packages(c(\
              "shinyFiles", \
              "shiny",
              "GGIR", \
              "jsonlite", \
              "DT", \
              "waiter", \
              "activityCounts", \
              "remotes" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/latest"\
          dependencies = TRUE)'
R -e 'remotes::install_github("rstudio/bslib")' # development version because CRAN version has bug that affects us
R -e 'remotes::install_github("habitus-eu/HabitusGUI")'