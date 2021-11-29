FROM continuumio/miniconda3:4.10.3

LABEL software="HabitusGUI" \
      author="Vincent van Hees <v.vanhees@accelting.com>, Emiliano Molinaro <molinaro@imada.sdu.dk>" \
      version="0.1.2" \
      license="MIT, Apache-2.0, LGPL-2.0" \
      description="HabitusGUI is a Shiny app that aids the researcher to work with \
                   to work with PALMSpy, GGIR, and activityCount.\
                   This dockerfile installs both the app and all it's depenencies."

MAINTAINER Vincent van Hees <v.vanhees@accelting.com>

###################
## PALMSpy
## The following lines of code  should remain identical to
## https://github.com/habitus-eu/PALMSpy/blob/master/Docker/Dockerfile
## Do not edit without editing the orignal

ARG VERSION=${VERSION:-1.9.6}

SHELL ["/bin/bash",  "--login", "-c"]

## Install software

RUN apt-get update \
 && apt-get install -y git vim nano emacs build-essential --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp

RUN git clone https://github.com/emolinaro/PALMSpy.git

WORKDIR PALMSpy

ENV PATH /opt/conda/envs/palmspy/bin:$PATH
ENV CONDA_PREFIX="/opt/conda/envs/palmspy"

RUN conda create -n palmspy python=3.7 openjdk=8.0 make=4.2.1 -y \
 && conda activate palmspy \
 && echo "source activate palmspy" > ~/.bashrc \
 && make \
 && make install \
 && make check \
 && make clean

WORKDIR /

###################
## HabitusGUI and it's dependencies, such as R packages GGIR and activityCounts

# get shiny server and R from the rocker project
FROM rocker/shiny

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
              "activityCounts", \
              "remotes" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/latest"\
          )'
RUN R -e 'remotes::install_github("habitus-eu/HabitusGUI", ref = "adding_real_palmspy")'

RUN mkdir -p /code

# run app by running the script
COPY /code/app.R /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]
