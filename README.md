
<!--
---
output:
   html_document:
     self_contained: no
---
-->
<!-- README.md is generated from README.Rmd. Please edit that file -->
nvdr
====

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

R package *nvdr* provides forecasts of monthly mean CVSS scores of subsets of CWE vulnerabilities. It uses data from [National Vulnerability Database (NVD)](https://nvd.nist.gov/).

Installation
------------

Install *nvdr* from GitHub.

``` r
install.packages("remotes")
remotes::install_github("realerikrani/nvdr")
```

Examples
--------

``` r
library(nvdr)
## Create new forecasting object.
cf <- CVSSForecaster$new()

## Find interesting CWE vulnerability categories between years 2011 and 2016.
cf$setCWENamesAndSeries()

## Set models (creates the forecasts models for the selected CWEs' time series and measures the accuracy).
cf$setARIMA()
cf$setETS()

## Get forecast plots.
cf$getPlots(cf$getARIMA())
cf$getPlots(cf$getETS())

## Get ARIMA model's AICc, AIC and BIC.
cf$getInformationCriterions(cf$getARIMA())

## Get forecast accuracy measures.
cf$getAssessments(cf$getARIMA())

## Available
   setBenchmark ## (Pick best comparing Naive, Drift, Seasonal Naive and Mean).
   setARIMA()
   setETS()
   setTSLinear()
   setNNAR()
   setARFIMA()
   setBaggedETS()
   setTBATS()
   setStructTS()
```

Look at the included binary data.

``` r
head(nvd)
```

License
-------

This package is licensed under GPL-3.

Acknowledgments
---------------

The forecasts of *nvdr* rely on the methods provided by R package *forecast*.

    @Manual{,
      title = {{forecast}: Forecasting functions for time series and linear models},
      author = {Rob J Hyndman},
      year = {2017},
      url = {http://pkg.robjhyndman.com/forecast},
    }

Furthermore, residuals are checked in *nvdr* by using the ideas from <https://github.com/robjhyndman/forecast/blob/c87f33/R/checkresiduals.R>. The exact way of lag calculations from that file are used in *nvdr*.

Notes
-----

The package includes preprocessed data obtained from XML Version 2.0 data from <https://nvd.nist.gov/vuln/data-feeds#CVE_FEED> as of 7 October 2017 covering vulnerability entries from CVE-2011 to CVE-2016. Users have the possibility to extract data on their own as well with the package's function `get_nvd_entries`(for example, `get_nvd_entries(c("nvdcve-2.0-2013.xml","nvdcve-2.0-2015.xml"))`)
