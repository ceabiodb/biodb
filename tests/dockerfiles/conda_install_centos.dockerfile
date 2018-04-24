FROM centos:latest

MAINTAINER Pierrick Roger ( pk.roger@icloud.com )

RUN curl -O https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
RUN yum install -y bzip2
RUN bash Miniconda3-latest-Linux-x86_64.sh -b -p "/miniconda3"
ENV PATH="/miniconda3/bin:$PATH"

RUN conda create -n biodb r-rcpp
RUN bash -c ". activate biodb && conda install r-xml r-stringr r-plyr r-jsonlite r-bitops r-rcurl r-r.utils r-digest r-codetools"
RUN yum install -y make

ADD . /biodb
RUN bash -c ". activate biodb && R CMD INSTALL --build /biodb"
