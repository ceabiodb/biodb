FROM alpine:latest

MAINTAINER Pierrick Roger ( pk.roger@icloud.com )

RUN wget -O miniconda-install.sh https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
RUN apk update
RUN apk add bash
RUN pwd
RUN ls
RUN whoami
RUN bash miniconda-install.sh -b -p "/miniconda3"
RUN pwd
RUN ls
ENV PATH="/miniconda3/bin:$PATH"
