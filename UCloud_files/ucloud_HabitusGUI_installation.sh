# Install conda
cd /tmp
wget -q  https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
chmod +x Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh -b
echo eval "$(~/miniconda3/bin/conda shell.bash hook)" >> ~/.bashrc
eval "$(~/miniconda3/bin/conda shell.bash hook)"
# now update conda
conda update conda -y

# Install git and git clone PALMSpy repo
conda install -c anaconda git
git clone https://github.com/emolinaro/PALMSpy.git

# Build PALMSpy
cd PALMSpy

# Python / PALMSpy dependencies
conda create -n palmspy python=3.7 openjdk=8.0 make=4.2.1
conda activate palmspy
make
make install
make clean

# Create symoblic link to be able to access all data in work work
sudo mkdir /srv/shiny-server/data
sudo chown -R ucloud:ucloud /srv/shiny-server/data
ln -s /work/TestDataHabitus2022/*  /srv/shiny-server/data/

# R / HabitusGUI dependencies
sudo apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev

R -e 'install.packages(c("shinyFiles", "shiny", "GGIR", "jsonlite", "DT", "waiter", "activityCounts", "remotes"), repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest", dependencies = TRUE)'
R -e 'remotes::install_github("rstudio/bslib")' # development version because CRAN version has bug that affects us
R -e 'remotes::install_github("habitus-eu/HabitusGUI")'