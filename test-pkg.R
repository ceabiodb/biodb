#!/usr/bin/env Rscript
# vi: ft=R fdm=marker

# CONSTANTS {{{1
################################################################

args <- commandArgs(trailingOnly = F)
SCRIPT.PATH <- sub("--file=","",args[grep("--file=",args)])
SCRIPT.DIR <- dirname(SCRIPT.PATH)

USER.AGENT <- "r-biodb.test ; pierrick.roger@gmail.com"

# MAIN {{{1
################################################################

# Run tests
devtools::test(SCRIPT.DIR)
