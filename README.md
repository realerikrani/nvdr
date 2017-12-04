
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
## Create a new CWE object.
c <- CWE$new()
## Set the data from CVE-2011 to CVE-2016 that come along with the package ...
c$setBaseData()
## ... or set data from selected XML Version 2.0 files downloaded from https://nvd.nist.gov/vuln/data-feeds#CVE_FEED .
c$setBaseData(c("nvdcve-2.0-2013.xml","nvdcve-2.0-2015.xml"))
## Get the year and month that mark the beginning of the interested time period.
c$getStartYear()
c$getStartMonth(as_number = T)
## Get the year and month that mark the end of the interested time period. See how to change the end and start at the end of the README.
c$getEndYear()
c$getEndMonth(as_number = T)
## Set monthly average CVSS time series for interesting CWEs ...
c$setTimeSeriesData(c$getInterestingMonthlyData())
## ... or set monthly average CVSS time series for specific CWEs.
c$setTimeSeriesData(c$getMonthlyData(c("CWE-119","CWE-78")))
## Create new forecasting object.
cf <- CVSSForecaster$new()
## Use the created CWE object with its time period and time series data.
cf$setCWEs(c)
## Create models and forecasts (creates the forecasts models for the selected CWEs' time series and measures the accuracy).
cf$setBenchmark()
cf$setETS()
cf$setARIMA()
## Get forecast plots.
cf$getPlots(cf$getARIMA())
cf$getPlots(cf$getETS())
## Get all created ARIMA models
cf$getARIMA()
## Get ARIMA model by CWE that is among the selected CWEs.
cf$getARIMA("CWE-119")
## Merge a potentially good ARIMA model's training and test data into new training data for forecasts of the unknown future of 9 months.
cf$getARIMA("CWE-119")$useModel(9)
## Save the unknown future results and plot them with ggplot2
u <- cf$getARIMA("CWE-119")$useModel(9)
ggplot2::autoplot(u)
## Get ARIMA model's AICc, AIC and BIC.
cf$getInformationCriterions(cf$getARIMA())
## Get ARIMA forecast accuracy measures.
cf$getAssessments(cf$getARIMA())
## Compare Benchmark and ETS
cf$compareAssessments(cf$getBenchmark(),cf$getETS())
## Get assessments of the most accurate models that have been created so far for each CWE
cf$getBestAssessments()
## Get all forecast accuracy measures so far
cf$getAllAssessments()
## Save a list of current models with best forecast accuracy.
cf$setBestModelList()
## Plot the best models list
cf$getPlots((cf$getBestModelList()))
## Merge the best models' (for each CWE based on the best forecast accuracy) training and test data into new training data for forecasts of the unknown future of 9 months.
list_of_unseen_future_forecasts <- cf$useBest(9)
## Plot the used best models' new forecasts
cf$plotUseBest(list_of_unseen_future_forecasts, row_no =5, col_no = 2)

## Available subset of methods for class CVSSForecaster. See the source code to understand specific use cases.
   setBenchmark() ## (Pick best comparing Naive, Drift, Seasonal Naive and Mean).
   setARIMA()
   setETS()
   setTSLinear()
   setNNAR()
   setARFIMA()
   setBaggedETS()
   setTBATS()
   setStructTS()
   getCWENames()
   getSeries()
   getBest()
   getAllAssessments()
   
## Available for class CWE. See the source code to understand specific use cases.
  getStartYear()
  getEndYear(plus_one = F)
  getStartMonth(as_number = F)
  getEndMonth(as_number = F, plus_one = F)
  getBaseData()
  getTimeSeriesData()
  setStartYear(year)
  setEndYear(year)
  setStartMonth(smonth)
  setEndMonth(emonth)
  setBaseData(files)
  setTimeSeriesData(ts_data)
  getAnalysisData()
  getMostInstances(threshold = 100)
  getMostCritical(min_score = 4.0)
  getChanging(period_threshold = 200)
  getInterestingMonthlyData(threshold = 100, min_score = 4.0, period_threshold = 200, as_monthly_ts = T)
  getMonthlyData(desired_cwes)
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

The package includes preprocessed data obtained from XML Version 2.0 data from <https://nvd.nist.gov/vuln/data-feeds#CVE_FEED> as of 7 October 2017 covering vulnerability entries from CVE-2011 to CVE-2016. Users have the possibility to extract data on their own without creating any objects as well with the package's function `get_nvd_entries`(for example, `get_nvd_entries(c("nvdcve-2.0-2013.xml","nvdcve-2.0-2015.xml"))`).
