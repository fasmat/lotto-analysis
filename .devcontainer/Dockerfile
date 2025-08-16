FROM r-base:4.5.1

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
        git \
        ssh

RUN R -e "install.packages('languageserver')" \
    && R -e "install.packages('minpack.lm')"
