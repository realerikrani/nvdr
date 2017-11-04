#' Binary NVD data fetcher
#'
#' Takes a subset of columns from binary NVD vulnerability data stored inside
#' the package. Makes sure the vulnerabilities are published between
#' given time limits.
#'
#' @keywords internal
#' @param start_year A number inidicating the starting year of the
#' time period (min 2011)
#' @param end_year A number indicating the ending year of the
#' time period (max 2016)
#'
#' @return A data.table with columns "published", "cwe", "cvss_score"
#' @export
#'
#' @examples
#' get_type_score_between(2011,2016)
get_type_score_between <- function(start_year, end_year) {
  if (start_year <= end_year) {
    nvd_selection <-
      stats::na.omit(nvdr::nvd[, c("published", "cwe", "cvss_score")])
    data.table::set(nvd_selection, j = "published",
                    value = as.Date(nvd_selection$published))
    limit_year <- as.Date(paste0(as.character(end_year + 1), "-01-01"))
    start_year_date <- as.Date(paste0(as.character(start_year), "-01-01"))
    data_between <- nvd_selection[which(
      nvd_selection$published < limit_year &
        nvd_selection$published >= start_year_date), ]
    data_between[order(data_between$published), ]
  } else {
    stop("start_year must be smaller or equal than end_year!")
  }
}



#' One-CWE and multi-CWE separator
#'
#' Separates one-CWE and multi-CWE vulnerability entries.
#'
#' @keywords internal
#' @param data A data.table with columns "published", "cwe" and "cvss_score".
#'
#' @return A list with two data.table elements: one containing entries
#' with simple CWEs, other entries with multiple CWEs.
#' @export
#'
#' @examples
#' \dontrun{discern_types(get_type_score_between(2011, 2012))}
discern_types <- function(data){
  mutli_cwe_condition <- grepl("|", data$cwe, fixed = TRUE)
  one_type_vulns <- data[!mutli_cwe_condition, ]
  multi_type_vulns <- data[mutli_cwe_condition, ]
  list(one_type_vulns, multi_type_vulns)
}


#' Year or Year and Month formatter
#'
#' Helps function \code{get_timegrouped_vulns} to group vulnerabilities
#' by months or by years by providing formatting.
#'
#' @keywords internal
#' @param dates Calendar Date of class "Date"
#' @param monthly Logical parameter indicating whether month and year format
#' is required.
#'
#' @return If parameter \code{monthly} is TRUE, then
#' \code{zoo::yearmon}'s months and years. Otherwise the number of the year
#' in the dates.
#' @export
#'
#' @examples
#' \dontrun{
#' date_formatter(c(as.Date("2011-01-01"),as.Date("2012-01-01")),monthly=TRUE)
#' }
date_formatter <- function(dates, monthly){
  if (monthly) {
    zoo::as.yearmon(dates)
  } else {
    as.numeric(format(zoo::as.yearmon(dates), "%Y"))
  }
}


#' CWE yearly or monthly characteristics creator
#'
#' For each CWE, finds the year's or month's sum of CVSS score,
#' the year's or month's count of the
#' CWE and calculates the year's or month's average CVSS score.
#'
#' @keywords internal
#' @param data A data.table with columns "published", "cwe" and "cvss_score".
#' @param monthly Logical parameter. If TRUE, then monthly grouping is done,
#' otherwise the data is grouped yearly.
#'
#' @return A data.frame with columns "published", "cwe", "cvss_sum", "count"
#' and "avg_cvss".
#' @export
#'
#' @examples
#' get_timegrouped_vulns(get_cwe_analysis_vulns(2011, 2012))
get_timegrouped_vulns <- function(data, monthly = FALSE){
  cwe <- cvss_score <- published <- count <- cvss_sum <- NULL
  plyr::ddply(
    data,
    plyr::.(published = date_formatter(published, monthly), cwe),
    plyr::here(plyr::summarise),
    cvss_sum = sum(as.numeric(cvss_score)), count = length(published),
    avg_cvss = ifelse(count != 0, round(cvss_sum / count, 1), 0)
  )
}


#' CWE organiser
#'
#' Organise every CWE by date.
#'
#' @keywords internal
#' @param one_type_vulns Vulnerabilities with only one CWE
#' @param multi_type_vulns Vulnerabilities with at least 2 CWE-s: CWE-A|CWE-B...
#'
#' @return A data.table with "published", "cwe" and "cvss_score" columns
#' @export
#'
#' @examples
#' \dontrun{
#' vulns <- discern_types(get_type_score_between(2011, 2012))
#' separate_and_merge(vulns[[1]], vulns[[2]])
#' }
separate_and_merge <- function(one_type_vulns, multi_type_vulns) {
  data.table::rbindlist(
    list(
      data.table::rbindlist(
        apply(multi_type_vulns, 1, function(vuln) {
          date <- vuln[1]
          cwes <- strsplit(vuln[2], "|", fixed = TRUE)
          cvss_score <- vuln[3]
          data.table::rbindlist(
            lapply(unlist(cwes), function(cwe) {
              list(published = as.Date(date), cwe = cwe,
                   cvss_score = cvss_score)
            }
            ))
        }
        )), one_type_vulns)
  )
}


#' Missing years filler
#'
#' Takes in the data of yearly vulnerability entries, for each selected CWE,
#' finds missing years and adds vulnerability count of 0 to there.
#'
#' @keywords internal
#' @param start_end A list containing \code{min} and \code{max} year from
#' \code{vulns_by_year$published}.
#' @param vulns_by_year A data.table output from
#' function \code{get_timegrouped_vulns}
#' @param selected_vulns_cwes A character vector with CWEs
#'
#' @return A data.table where all CWEs have a count for each year of the
#' selected time period.
#' @export
#'
#' @examples
#' \dontrun{
#' fill_missing_years_with_zero(list(overall_first_year, overall_last_year),
#' vulns_by_year,
#' cwe_year_above_200)
#' }
fill_missing_years_with_zero <- function(start_end, vulns_by_year,
                                         selected_vulns_cwes){
  published <- NULL
  missing <- data.table::rbindlist(lapply(selected_vulns_cwes, function(cwe) {
    cwe_specific_data <- vulns_by_year[vulns_by_year$cwe == cwe, ]
    first_year <- min(cwe_specific_data$published)
    last_year <- max(cwe_specific_data$published)
    missing_years <- setdiff(seq(start_end[[1]], start_end[[2]]),
                             seq(first_year, last_year))
    if (length(missing_years) > 0) {
      vulns_year_missing <- data.table::rbindlist(
        missing_years_with_zeros <- lapply(missing_years, function(year) {
          list(published = year, cwe = cwe, count = 0)
        }
        ))
    }
  }
  ))
  plyr::arrange(data.table::rbindlist(list(vulns_by_year, missing)), published)
}


#' Missing months filler
#'
#' Generates the missing months with zeros
#'
#' @keywords internal
#' @param start_end A list containing the minimum and maximum year of the time
#' period
#' @param vulns_by_month A data.table output from
#' function \code{get_timegrouped_vulns(data, monthly = TRUE)}
#' @param selected_vulns_cwes A character vector with CWEs
#'
#' @return A data.table where all CWEs have a count for each month of the
#' selected time period.
#' @export
#'
#' @examples
#' \dontrun{
#' vulns_by_month <- get_timegrouped_vulns(data, monthly = TRUE)
#' vulns_by_month <-
#'   fill_missing_months_with_zero(list(start_year, end_year), vulns_by_month,
#'                                   interesing_cwes)
#' }
fill_missing_months_with_zero <- function(start_end, vulns_by_month,
                                          selected_vulns_cwes) {
  published <- NULL
  months_no <- (start_end[[2]] - start_end[[1]] + 1) * 12
  full_dates <- zoo::as.yearmon(seq(
    as.Date(paste0(as.character(start_end[[1]]), "-01-01")),
    as.Date(paste0(as.character(start_end[[2]]), "-12-01")), by = "month"))
  missing <- data.table::rbindlist(lapply(selected_vulns_cwes, function(cwe) {
    if (!nrow(vulns_by_month[vulns_by_month$cwe == cwe, ]) == months_no) {
      missing_months <-
        zoo::as.yearmon(
          setdiff(full_dates,
                  vulns_by_month[vulns_by_month$cwe == cwe, "published"]))
      if (length(missing_months) > 0) {
        missing_months_with_zeros <-
          data.table::rbindlist(lapply(missing_months, function(month) {
            list(published = month, cwe = cwe, cvss_sum = 0, count = 0,
                 avg_cvss = 0)
          }
          ))
      }
    }
  }
  ))
  plyr::arrange(
    data.table::rbindlist(list(vulns_by_month, missing)), published)
}


#' Percent Change of instances (last year vs all others) finder
#'
#' |(Percent Change)| = |((New Amount) - (Base Amount)) / (Base Amount)|,
#' where (New Amount) is last year vulnerability instances count and
#' (Base Amount) is each of other years' vulnerability instances counts.
#' Finally, calculates a mean of the found percent change numbers for a
#' a given CWE.
#'
#' @keywords internal
#' @param selected_vulns_cwes A character vector with CWEs
#' @param vulns_by_year Output from function \code{fill_missing_years_with_zero}
#' or output \code{get_timegrouped_vulns}
#' @param last_year The given time period's final year.
#'
#' @return A data.table with columns "cwe" and "avg_change".
#' @export
#'
#' @examples
#' \dontrun{
#' find_yearly_mean_change(cwes_above_threshold, vulns_by_year_all, last_year)
#' }
find_yearly_mean_change <-
  function(selected_vulns_cwes, vulns_by_year, last_year){
    data.table::rbindlist(lapply(selected_vulns_cwes, function(cwe_name){
      cwe_specific_counts <-
        vulns_by_year[vulns_by_year$cwe == cwe_name, ]
      last_year_count <-
        cwe_specific_counts[cwe_specific_counts$published == last_year, "count"]
      other_year_counts <-
        cwe_specific_counts[cwe_specific_counts$published != last_year, "count"]
      new_amount <- last_year_count
      base_amount <- other_year_counts
      percent_change <-
        ifelse(base_amount == 0, 0,
               abs((new_amount - base_amount) / base_amount))
      list(cwe = cwe_name, avg_change = round(mean(percent_change), 2))
    }
    ))
  }
