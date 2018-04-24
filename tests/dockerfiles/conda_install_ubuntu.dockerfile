FROM ubuntu:latest

MAINTAINER Pierrick Roger ( pk.roger@icloud.com )

RUN apt-get update
RUN apt-get install -y wget
RUN wget -O miniconda-install.sh https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
RUN apt-get install -y bzip2
RUN bash miniconda-install.sh -b -p "/miniconda3"
ENV PATH="/miniconda3/bin:$PATH"

RUN conda create -n biodb r-rcpp
RUN bash -c ". activate biodb && conda install r-xml r-stringr r-plyr r-jsonlite r-bitops r-rcurl r-r.utils r-digest"
RUN apt-get install -y make
RUN bash -c ". activate biodb && conda install r-codetools"

ADD . /biodb
RUN bash -c ". activate biodb && R CMD INSTALL --build /biodb"
