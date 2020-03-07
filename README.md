<!-- README.md is generated from README.Rmd. Please edit that file -->

gotmtools
=====


Tools for interacting with the [General Ocean Turbulence Model (GOTM)](http://gotm.net/ "General Ocean Turbulence Model's website") in R. `gotmtools` is inspired by glmtools and applies many of the same functions but adapted for handling GOTM output. It is focused on dealing with model output and diagnosing model performance compared to observed data.

Example files can be accessed [here](https://github.com/tadhg-moore/GOTM_examples/) which will be converted into a vignette at some stage.
Function description soon to follow... 

## Installation

You can install gotmtools from Github with:

```{r gh-installation, eval = FALSE}
devtools::install_github("hdugan/glmtools")
# install.packages("devtools")
devtools::install_github("aemon-j/GOTMr")
devtools::install_github("aemon-j/gotmtools")
```
      
What libraries does `gotmtools` need?
===
This version requires the NetCDF version R library (called `ncdf4`), and `rLakeAnalyzer`, and `tools` if you would like to run all examples. 


