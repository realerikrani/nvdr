#' Monthly average CVSS time-series creator
#'
#' Convert processed data into a time series object
#'
#' @keywords internal
#' @param chosen_vulns A character vector with CWEs
#' @param vulns_by_month Vulnerabilities with monthly average CVSS scores
#' @param start_year The start year of the observed period
#'
#' @return A monthly time-series object <"mts" "ts" "matrix"> of
#' average CVSS scores of vulnerabilities with CWEs in columns
#' @export
vulns_into_time_series <-
  function(chosen_vulns, vulns_by_month, start_year) {
    vulns_monthly_ts <-
      lapply(chosen_vulns,
             function(cwe_name) {
               vulns_by_month[vulns_by_month$cwe == cwe_name, c("avg_cvss")]
             }
      )
    names(vulns_monthly_ts) <- chosen_vulns
    stats::ts(data.table::setDT(vulns_monthly_ts),
              frequency = 12, start = start_year)
  }
