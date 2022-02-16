# {{pkgName}}

An R package for accessing {{dbTitle}}, based on R package/framework [biodb](https://github.com/pkrog/biodb/).

## Introduction

This package is an extension of [biodb](https://github.com/pkrog/biodb/) that implements a connector to {{dbTitle}}.

## Installation

Install the latest version of this package by running the following commands:
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install('biodb')
devtools::install_github('{{githubRepos}}', dependencies=TRUE)
```

## Examples

To instantiate a connector to {{dbTitle}}, run:
```r
mybiodb <- biodb::Biodb()
conn <- mybiodb$getFactory()$createConn('{{dbName}}')
mybiodb$terminate()
```

## Documentation

To get documentation on the implemented connector, run the following command in R:
```r
?{{pkgName}}::{{connClass}}
```

## Citations

<!-- TODO -->
