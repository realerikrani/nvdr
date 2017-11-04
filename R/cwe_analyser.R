#' CWE categories with most instances finder
#'
#' Finds CWE categories, which have been discovered more times than the
#' threshold parameter's value in any of the years of the given time period.
#'
#' @param data A data.table with "published", "cwe" and "cvss_score" columns
#' @param threshold A number. Each CWE vulnerability included in the output
#' of this function must have the number of found vulnerabilities
#' (in any of the years of the time period) bigger than the \code{threshold}.
#'
#' @return A data.frame with columns "cwe" and "sum" (sum of the count of
#' vulnerabilities of the given time period)
#' @export
#'
#' @examples
#' cwes_most_instances(get_cwe_analysis_vulns())
cwes_most_instances <- function(data, threshold = 100){
  cwe <- count <- NULL
  cwe_count_by_year <- get_timegrouped_vulns(data)
  any_year_has_n <-
    plyr::ddply(
    cwe_count_by_year,
    plyr::.(cwe), plyr::here(plyr::summarise),
    any_over_n = any(count >= threshold), sum = sum(count))
  plyr::arrange(
    any_year_has_n[which(any_year_has_n$any_over_n == TRUE),
                   c("cwe", "sum")], -sum)
}


#' CWE categories with most critical impact finder
#'
#' Finds CWE categories, which have a higher or equal yearly
#' summed average CVSS score than the parameter \code{min_score}.
#'
#' @param data A data.table with "published", "cwe" and "cvss_score" columns
#' @param min_score A CVSS score number set to be the minimum that is of the
#' interest
#'
#' @return A data.frame with columns "cwe" and
#' "cvss" (summed year's average CVSS score that is divided by the length of
#' the given time period).
#' @export
#'
#' @examples
#' cwes_most_critical_impact(get_cwe_analysis_vulns())
cwes_most_critical_impact <- function(data, min_score = 4.0){
  cwe <- avg_cvss <- cvss <- NULL
  vulns_by_year <- get_timegrouped_vulns(data)
  year_range <- max(vulns_by_year[, "published"]) -
    min(vulns_by_year[, "published"]) + 1
  year_avg_cvss <- plyr::ddply(vulns_by_year,
                               plyr::.(cwe), plyr::here(plyr::summarise),
                               cvss = round(sum(avg_cvss) / year_range, 1))
  plyr::arrange(year_avg_cvss[year_avg_cvss$cvss > min_score, ], -cvss)
}


#' CWE categories with highest change-compared-to-the-last-year finder
#'
#' Finds yearly mean of absolute CVSS score percent change with function
#' \code{find_yearly_mean_change} and outputs the results that are above
#' the median of the change number or equal to the median.
#'
#' @param data A data.table with "published", "cwe" and "cvss_score" columns
#' @param period_threshold A number. Each CWE vulnerability included in the
#' output of this function must have the number of found vulnerabilities
#' (summed over the years of the time period) bigger than
#' the \code{period_threshold}.
#'
#' @return A data.table with columns "cwe" and "avg_change"
#' @export
#'
#' @examples
#' cwes_highest_instances_change(get_cwe_analysis_vulns())
cwes_highest_instances_change <- function(data, period_threshold = 200){
  avg_change <- cwe <- avg_cvss <- cvss <- NULL
  cwe_instance_counts <- cwes_most_instances(data = data, threshold = 0)
  cwes_above_period_threshold <-
    cwe_instance_counts[cwe_instance_counts$sum >= period_threshold, "cwe"]
  vulns_by_year <- get_timegrouped_vulns(data)[, c("published", "cwe", "count")]
  overall_first_year <- min(vulns_by_year$published)
  overall_last_year <- max(vulns_by_year$published)
  if (overall_first_year == overall_last_year) {
    data.table::data.table(cwe = cwes_above_period_threshold, avg_change = 0.00)
  } else {
    vulns_by_year_all <- fill_missing_years_with_zero(
      list(overall_first_year, overall_last_year), vulns_by_year,
      cwes_above_period_threshold)
    vulns_mean_change <- find_yearly_mean_change(cwes_above_period_threshold,
                                            vulns_by_year_all,
                                            overall_last_year)
    vulns_median <- stats::median(vulns_mean_change$avg_change)
    plyr::arrange(
      vulns_mean_change[vulns_mean_change$avg_change >= vulns_median, ],
      -avg_change)
  }
}


#' CWE analysis data organiser
#'
#' Gets the vulnerability data of the given time period. Makes sure all the
#' CWEs are separated.
#'
#' @param start_year A number inidicating the starting year of the
#' time period (min 2011)
#' @param end_year A number indicating the ending year of the
#' time period (max 2016)
#'
#' @return A data.table with "published", "cwe" and "cvss_score" columns
#' @export
#'
#' @examples
#' get_cwe_analysis_vulns(2012,2014)
get_cwe_analysis_vulns <- function(start_year=2011, end_year=2016) {
  data <- get_type_score_between(start_year, end_year)
  vulns <- discern_types(data)
  separate_and_merge(vulns[[1]], vulns[[2]])
}


#' Interesting CWE categories finder
#'
#' Merges the CWEs found with functions \code{cwe_most_instances},
#' \code{cwes_most_critical_impact} and
#' \code{cwes_highest_instances_change} and returns the list of the found CWEs.
#'
#' @param start_year A number inidicating the starting year of the
#' time period (min 2011 and default 2011).
#' @param end_year A number indicating the ending year of the
#' time period (max 2016 and default 2016).
#' @param threshold A number. Each CWE vulnerability included in the output
#' of \code{cwes_most_instances} function must have the number of
#' found vulnerabilities (default 100).
#' (in any of the years of the time period) bigger than the \code{threshold}.
#' @param min_score A CVSS score number set to be the minimum that is of the
#' interest (default 4.0).
#' @param period_threshold A number. Each CWE vulnerability included in the
#' output of function \code{cwes_highest_instances_change} must have the
#' number of found vulnerabilities.
#' (summed over the years of the time period) bigger than
#' the \code{period_threshold} (default 200).
#' @param as_monthly_ts Logical value. When TRUE
#'
#' @return A character vector of CWE names (default). If
#' parameter \code{as_monthly_ts} is TRUE, then a monthly time-series
#' object <"mts" "ts" "matrix"> of average CVSS scores of vulnerabilities with
#' CWEs in columns are returned instead.
#' @export
#'
#' @examples
#' get_interesting_cwes()
#' get_interesting_cwes(start_year=2014, end_year=2015, as_monthly_ts = TRUE)
get_interesting_cwes <- function(start_year=2011, end_year=2016,
                                 threshold = 100, min_score = 4.0,
                                 period_threshold = 200, as_monthly_ts = T){
  data <- get_cwe_analysis_vulns(start_year, end_year)
  vulns_most_instances <- cwes_most_instances(data, threshold = threshold)
  vulns_most_impact <- cwes_most_critical_impact(data, min_score = min_score)
  vulns_most_change <-
    cwes_highest_instances_change(data, period_threshold =  period_threshold)
  interesing_cwes <- unique(c(vulns_most_instances$cwe,
                              vulns_most_impact$cwe,
                              vulns_most_change$cwe))
  if (as_monthly_ts) {
    vulns_by_month <- get_timegrouped_vulns(data, monthly = TRUE)
    vulns_by_month <-
      fill_missing_months_with_zero(list(start_year, end_year), vulns_by_month,
                                    interesing_cwes)
    vulns_into_time_series(interesing_cwes, vulns_by_month, start_year)
  } else {
    interesing_cwes
  }
}
