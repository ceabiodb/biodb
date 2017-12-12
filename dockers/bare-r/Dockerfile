FROM ubuntu:14.04

MAINTAINER Pierrick Roger ( pk.roger@icloud.com )

RUN echo "deb http://cran.univ-paris1.fr/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

RUN apt-get update

RUN apt-get -y install r-base libcurl4-openssl-dev libxml2-dev git
RUN R -e "install.packages('devtools', dependencies = TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('pkrog/biodb', ref = 'develop')"

ENTRYPOINT ["Rscript"]
