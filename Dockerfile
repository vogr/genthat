FROM prlprg/r-full

ADD . /genthat

RUN Rscript -e "install.packages('/genthat', repos=NULL)"

WORKDIR /genthat
