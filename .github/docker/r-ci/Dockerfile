FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    git \
    make \
    gcc \
    g++ \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "options(\
  repos = c(CRAN = 'https://cloud.r-project.org'),\
  keep.source.pkgs = TRUE,\
  keep.parse.data.pkgs = TRUE,\
  Ncpus = max(1L, parallel::detectCores() - 1L)\
); install.packages(\
  c('ggplot2', 'dplyr', 'stringr', 'testthat'),\
  dependencies = NA,\
  INSTALL_opts = c('--with-keep.source', '--with-keep.parse.data')\
)"

WORKDIR /work
