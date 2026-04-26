FROM mcr.microsoft.com/devcontainers/base:noble

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive && \
    apt-get -y install --no-install-recommends \
        build-essential \
        r-base-dev \
        r-recommended 

RUN R -e "install.packages('languageserver')" && \
    R -e "install.packages('minpack.lm')" && \
    rm -rf /var/lib/apt/lists/*
