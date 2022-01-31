FROM continuumio/miniconda3:4.10.3 as builder

LABEL software="HabitusGUI" \
      author="Vincent van Hees <v.vanhees@accelting.com>, Emiliano Molinaro <molinaro@imada.sdu.dk>" \
      version="0.1.2" \
      license="MIT, Apache-2.0, LGPL-2.0" \
      description="HabitusGUI is a Shiny app that aids the researcher to work with \
                   PALMSpy, GGIR, and activityCount.\
                   This dockerfile installs both the app and all it's depenencies."

MAINTAINER Vincent van Hees <v.vanhees@accelting.com>

###################
## PALMSpy
## The following lines of code should remain consistent with
## https://github.com/habitus-eu/PALMSpy/blob/master/Docker/Dockerfile
## Do not edit without editing the orignal

ARG VERSION=${VERSION:-1.9.6}

SHELL ["/bin/bash",  "--login", "-c"]

## Install software

RUN apt-get update \
 && apt-get install -y git build-essential --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp

RUN git clone https://github.com/emolinaro/PALMSpy.git

WORKDIR PALMSpy

ENV PATH /opt/conda/envs/palmspy/bin:$PATH
ENV CONDA_PREFIX="/opt/conda/envs/palmspy"

RUN conda create -n palmspy python=3.7 openjdk=8.0 make=4.2.1 -y \
 && conda activate palmspy \
 && make \
 && make install \
 && make check \
 && make clean

WORKDIR /

###################
## HabitusGUI and it's R dependencies, such as R packages GGIR and activityCounts

# get shiny server and R from the rocker project
FROM rocker/shiny

SHELL ["/bin/bash",  "--login", "-c"]

COPY --from=builder /opt/conda /opt/conda

ENV PATH /opt/conda/envs/palmspy/bin:$PATH
ENV CONDA_PREFIX="/opt/conda/envs/palmspy"

# system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev 

# install R packages required
RUN R -e 'install.packages(c(\
              "shinyFiles", \
              "GGIR", \
              "jsonlite", \
              "DT", \
              "waiter", \
              "activityCounts", \
              "remotes" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/latest"\
          )'
RUN R -e 'remotes::install_github("rstudio/bslib")' # development version because CRAN version has bug that affects us
RUN R -e 'remotes::install_github("habitus-eu/HabitusGUI", ref = "adding_real_palmspy")'

# Copy code for running the app to the container
RUN mkdir -p /code
COPY /code/app.R /srv/shiny-server/

# Change working directory to equal the location of the mounted volume
WORKDIR /srv/shiny-server/data

# run app
CMD ["/usr/bin/shiny-server"]

