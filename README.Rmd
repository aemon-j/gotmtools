---
output: 
  github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
```

gotmtools
=====


Tools for interacting with the [General Ocean Turbulence Model (GOTM)](http://gotm.net/ "General Ocean Turbulence Model's website") in R. `gotmtools` is inspired by glmtools and applies many of the same functions but adapted for handling GOTM output. It is focused on dealing with model output and diagnosing model performance compared to observed data. 

`gotmtools` Functions
=====
```{r load printr, echo=FALSE,message=FALSE,results='hide'}
loadNamespace("printr")
```

```{r echo=FALSE}
help.search("*", package = "gotmtools", types = "help")
```

```{r unload printr, echo=FALSE}
unloadNamespace("printr")
```

## Installation

You can install gotmtools from Github with:

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("tadhg-moore/gotmtools")
```

        
What libraries does `gotmtools` need?
===
This version requires the NetCDF version R library (called `ncdf4`), and `rLakeAnalyzer`, and `tools` if you would like to run all examples. 

```{r echo=FALSE}
deps <- desc::desc_get_deps()
knitr::kable(deps[deps$type %in% c("Imports", "Depends"), 2:3])
```

