
FROM r-base

RUN apt-get -y update && apt-get -y install libssl-dev libcurl4-openssl-dev libxml2-dev

RUN Rscript -e 'install.packages(c("devtools", "pryr"))'

# IMPORTS
RUN Rscript -e 'install.packages(c("Rcpp", "codetools", "testthat", "covr"))'

# SUGGESTS
RUN Rscript -e 'install.packages(c("knitr", "rmarkdown", "roxygen2"))'

