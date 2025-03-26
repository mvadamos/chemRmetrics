# chemRmetrics

## Multivariate Statistical Analysis of Chemical Data

This is an accessible package in R that can be used to perform multivaraite statistical analysis on chemical data. 

Currently with this package, you can:

* Import data from appropriated formatted excel spreadsheet
* Import data from appropriately labelled JCAMP-DX files
* Perform a range of data pre-processing:
  * Truncating
  *  Interpolation
  * Normalising to unity
* Perform Principal Component Analysis (PCA)
* Perform a projection of a separate data set onto an existing PCA model
* Create 2D PCA scores plots
* Create interactable 3D PCA scores plots
* Create a rotating gif of 3D PCA scores plots
* Create scree plots
* Create loadings plots
* Export all data associated with the PCA model to plot in a software of your choice :)

## Read this document to learn how to use chemRmetrics!

[chemRmetrics User Guide!](chemRmetrics-User-Guide.html) (Download to view file as it is too large to view in github)

## How to Install the chemRmetrics Package

Here is the easiest way to install the chemRmetrics package:

1. Install the devtools package - `install.packages('devtools')`
2. Install chemRmetrics package using devtools package - `devtools::install_github('mvadamos/chemRmetrics')`

## Known Issues (Work in Progress)

Here are a few known issues that have been encountered when using the chemRmetrics package:

 * Hidden files may interfere with `load_data()` function when loadings JDX files. Might throw error 'This file is not a jdx file', the hidden file should be visible in Rstudio file view, just remove file to solve issue.
 * Computer firewall may prevent `create_3D_gif()` function form operating properly, usually will produce a blank gif file. Not sure how to resolve this issue, it is something to do with webshot, this function does not work on my 'work computer' but it does work on my 'personal computer'.
