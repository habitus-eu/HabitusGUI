#!/bin/bash

set -ex

# Install conda
cd /tmp
wget -q  https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
chmod +x Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh -b
echo eval "$(~/miniconda3/bin/conda shell.bash hook)" >> ~/.bashrc
eval "$(~/miniconda3/bin/conda shell.bash hook)"
# now update conda
conda update conda -y

git clone https://github.com/emolinaro/PALMSpy.git

# Build PALMSpy
cd PALMSpy

# Python / PALMSpy dependencies
conda create -n palmspy python=3.7 openjdk=8.0 make=4.2.1 -y
conda activate palmspy
make
make install
make clean

echo "conda activate palmspy" >> ~/.bashrc

# Create symoblic link to be able to access all data in work 
sudo mkdir /srv/shiny-server/data
sudo chown -R ucloud:ucloud /srv/shiny-server/data
ln -s "/work/Member Files: LineMatthiesen#8897/*"  /srv/shiny-server/data/

# R / HabitusGUI dependencies
sudo apt-get update && sudo apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libgdal-dev \
    libudunits2-dev

R -e 'install.packages(c("shinyFiles", "shiny", "GGIR", "jsonlite", "DT", "waiter"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
R -e 'install.packages(c("activityCounts", "remotes", "shinyjs", "dplyr", "magrittr"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
R -e 'install.packages(c("sf", "readr", "tidyr", "stringr", "sp", "raster", "lwgeom", "tidyverse"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
R -e 'remotes::install_github("rstudio/bslib", dependencies = TRUE)' # development version because CRAN version has bug that affects us
R -e 'remotes::install_github("vincentvanhees/palmsplusr", ref = "config_file", dependencies = TRUE)'
R -e 'remotes::install_github("habitus-eu/HabitusGUI", dependencies = TRUE)'


# Assuming that app.R is in the same folder as this script
APP_PATH=$(find /work/ -name app.R)
FOLDER=$(dirname "$APP_PATH")

# configure shiny-server
sudo sed -i "s+myshinyapp-dir+$FOLDER+" /etc/shiny-server/shiny-server.conf

## launch shiny-server
shiny-server

