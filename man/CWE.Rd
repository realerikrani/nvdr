% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cwe.R
\name{CWE}
\alias{CWE}
\title{Class CWE}
\description{
Manages the data that forms the basis of the forecasts.
}
\section{Private attributes}{
{
\itemize{
  \item \strong{start_year} the first year of a time period (default 2011)
  \item \strong{end_year} the last year of a time period (default 2016)
  \item \strong{start_month} the first month of a time period (default 1)
  \item \strong{end_month} the last month of a time period (default 12)
  \item \strong{base_data} the attribute for holding data extracted from XML (default NA)
  \item \strong{time_series_data} the attribute for holding the \code{stats::ts} time series type of data after processing \code{base data} (default NA)
  \item \strong{pub_cwe_cvss = c("published", "cwe", "cvss_score")} a \code{character} vector attribute used in the source code
  \item \strong{pub_cwe_count =  c("published", "cwe", "count")} a \code{character} vector attribute used in the source code
  \item \strong{published = "published"} a \code{character} attribute used in the source code
}
}
}

\section{Public methods}{
{
\itemize{
  \item \strong{new()} creates new CWE object
  \item \strong{getStartYear()} returns private attribute \code{start_year}
  \item \strong{getEndYear(plus_one = F)} returns private attribute \code{end_year}; adds one if \code{plus_one = T}
  \item \strong{getStartMonth(as_number = F)} returns a \code{character} of private attribute \code{start_month} (appends zero to months 1...9); returns private attribute \code{start_month} if \code{as_number = T}
  \item \strong{getEndMonth(as_number = F, plus_one = F)} returns a \code{character} of private attribute \code{end_month} (appends zero to months 1...9); returns private attribute \code{end_month} if \code{as_number = T}; adds one if \code{plus_one = T}
  \item \strong{getBaseData()} returns private attribute \code{base_data}
  \item \strong{getTimeSeriesData()} returns private attribute \code{time_series_data}
  \item \strong{setStartYear(year)} assings the \code{numeric} value of \code{year} to private attribute \code{start_year}
  \item \strong{setEndYear(year)} assings the \code{numeric} value of \code{year} to private attribute \code{end_year}
  \item \strong{setStartMonth(smonth)} assings the \code{numeric} value of \code{smonth} to private attribute \code{start_month}
  \item \strong{setEndMonth(emonth)} assings the \code{numeric} value of \code{emonth} to private attribute \code{end_month}
  \item \strong{setBaseData(files)} calls \link{get_nvd_entries}(files) and assings the output to private attribute \code{base_data}; if the argument \code{files} is missing, then assings \link{nvd} to \code{base_data}
  \item \strong{setTimeSeriesData(ts_data)} assigns the \link[stats]{ts} value of \code{ts_data} to private attribute \code{time_series_data}
  \item \strong{getAnalysisData()} returns a \link[data.table]{data.table} where CWEs are between the time limits set by private attributes, where each entry corresponds to one CWE and where each CWE has the date when it was published and its CVSS score (NAs are omitted)
  \item \strong{getMostInstances(threshold = 100)} returns CWEs which are counted at least \code{threshold} times in a year belonging to the time period set by private attributes
  \item \strong{getMostCritical(min_score = 4.0)} returns CWEs which have higher mean yearly mean (found by using summed yearly mean CVSS scores and the years count of the time period) severity score than \code{min_score}
  \item \strong{getChanging(period_threshold = 200)} returns CWEs with a total count of at least \code{period_threshold} which mean absolute percentage change (count of the last year vs counts of other years) is higher than the median of the calculated mean absolute percentage change numbers.
  \item \strong{getInterestingMonthlyData(threshold = 100, min_score = 4.0, period_threshold = 200, as_monthly_ts = T)} returns the union of unique CWEs found by methods \code{getChanging}, \code{getMostCritical} and \code{getMostInstances}. Output is monthly time series with mean monthly CVSS scores when \code{as_monthly_ts = T}, otherwise a \code{character} vector of CWE IDs.
  \item \strong{getMonthlyData(desired_cwes)} returns monthly time series with mean monthly CVSS scores of all available CWEs when \code{desired_cwes} is missing, otherwise only for those specified in the \code{character} vector \code{desired_cwes}
}
}
}

