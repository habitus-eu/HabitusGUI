# WARNING THESE INSTALLATION INSTRUCTIONS MAY NO LONGER WORK
# WE ARE KEEPING THIS FILE FOR TESTING AND DEVELOPMENT PURPOSES

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
ln -s "/work/Member Files: VincentVanHees/*"  /srv/shiny-server/data/

# R / HabitusGUI dependencies
sudo apt-get update && sudo apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libgdal-dev \
    libudunits2-dev

# Shiny dependencies
R -e 'install.packages(c("shinyFiles", "shiny", "jsonlite", "DT", "waiter", "bslib"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
# GGIR and its dependencies
R -e 'install.packages(c("GGIR", "actilifecounts", "ActCR", "GGIRread", "read.gt3x"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
# palmsplusr dependencies
R -e 'install.packages(c("remotes", "shinyjs", "dplyr", "magrittr"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
R -e 'install.packages(c("sf", "readr", "tidyr", "stringr", "sp", "raster", "lwgeom", "tidyverse"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
# palmsplusr itself
R -e 'remotes::install_github("vincentvanhees/palmsplusr", dependencies = TRUE)'
# The HabitusGUI itself
R -e 'remotes::install_github("habitus-eu/HabitusGUI", dependencies = TRUE)'


# Assuming that app.R is in the same folder as this script
APP_PATH=$(find /work/ -name app.R)
FOLDER=$(dirname "$APP_PATH")

# configure shiny-server
sudo sed -i "s+myshinyapp-dir+$FOLDER+" /etc/shiny-server/shiny-server.conf

## launch shiny-server
shiny-server

