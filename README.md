<!-- README.md is generated from README.Rmd. Please edit that file -->

gotmtools
=====


Tools for interacting with the [General Ocean Turbulence Model (GOTM)](http://gotm.net/ "General Ocean Turbulence Model's website") in R. `gotmtools` is inspired by glmtools and applies many of the same functions but adapted for handling GOTM output. It is focused on dealing with model output and diagnosing model performance compared to observed data.
Function desription and example files soon to follow... 

## Installation

You can install gotmtools from Github with:

```{r gh-installation, eval = FALSE}
install.packages('glmtools', repos = "https://owi.usgs.gov/R")
# install.packages("devtools")
devtools::install_github("tadhg-moore/GOTMr")
devtools::install_github("tadhg-moore/gotmtools")
```
      
What libraries does `gotmtools` need?
===
This version requires the NetCDF version R library (called `ncdf4`), and `rLakeAnalyzer`, and `tools` if you would like to run all examples. 


