FROM r-base:latest

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
        git \
        ssh \
        libcurl4-openssl-dev \
        libxml2-dev

RUN R -e "install.packages('languageserver')" \
    && R -e "install.packages('minpack.lm')"
