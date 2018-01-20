#' Class CWE
#'
#' Manages the data that forms the basis of the forecasts.
#
#'
#' @section Private attributes:{
#' \itemize{
#'   \item \strong{start_year} the first year of a time period (default 2011)
#'   \item \strong{end_year} the last year of a time period (default 2016)
#'   \item \strong{start_month} the first month of a time period (default 1)
#'   \item \strong{end_month} the last month of a time period (default 12)
#'   \item \strong{base_data} the attribute for holding data extracted from XML (default NA)
#'   \item \strong{time_series_data} the attribute for holding the \code{stats::ts} time series type of data after processing \code{base data} (default NA)
#'   \item \strong{pub_cwe_cvss = c("published", "cwe", "cvss_score")} a \code{character} vector attribute used in the source code
#'   \item \strong{pub_cwe_count =  c("published", "cwe", "count")} a \code{character} vector attribute used in the source code
#'   \item \strong{published = "published"} a \code{character} attribute used in the source code
#' }
#' }
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{new()} creates new CWE object
#'   \item \strong{getStartYear()} returns private attribute \code{start_year}
#'   \item \strong{getEndYear(plus_one = F)} returns private attribute \code{end_year}; adds one if \code{plus_one = T}
#'   \item \strong{getStartMonth(as_number = F)} returns a \code{character} of private attribute \code{start_month} (appends zero to months 1...9); returns private attribute \code{start_month} if \code{as_number = T}
#'   \item \strong{getEndMonth(as_number = F, plus_one = F)} returns a \code{character} of private attribute \code{end_month} (appends zero to months 1...9); returns private attribute \code{end_month} if \code{as_number = T}; adds one if \code{plus_one = T}
#'   \item \strong{getBaseData()} returns private attribute \code{base_data}
#'   \item \strong{getTimeSeriesData()} returns private attribute \code{time_series_data}
#'   \item \strong{setStartYear(year)} assings the \code{numeric} value of \code{year} to private attribute \code{start_year}
#'   \item \strong{setEndYear(year)} assings the \code{numeric} value of \code{year} to private attribute \code{end_year}
#'   \item \strong{setStartMonth(smonth)} assings the \code{numeric} value of \code{smonth} to private attribute \code{start_month}
#'   \item \strong{setEndMonth(emonth)} assings the \code{numeric} value of \code{emonth} to private attribute \code{end_month}
#'   \item \strong{setBaseData(files)} assings the output from \link{get_nvd_entries}(files) to private attribute \code{base_data}; if the argument \code{files} is missing, then assings \link{nvd} to \code{base_data}
#'   \item \strong{setTimeSeriesData(ts_data)} assigns the \link[stats]{ts} value of \code{ts_data} to private attribute \code{time_series_data}
#'   \item \strong{getAnalysisData()} return a \link[data.table]{data.table} where CWEs are between the time limits set by private attributes, where each entry corresponds to one CWE and where each CWE has the date when it was published and its CVSS score (NAs are omitted)
#'   \item \strong{getMostInstances(threshold = 100)} returns CWEs which are counted at least \code{threshold} times in a year belonging to the time period set by private attributes
#'   \item \strong{getMostCritical(min_score = 4.0)} returns CWEs which have higher mean yearly mean (found by using summed yearly mean CVSS scores and the years count of the time period) severity score than \code{min_score}
#'   \item \strong{getChanging(period_threshold = 200)} returns CWEs with a total count of at least \code{period_threshold} which mean absolute percentage change (count of the last year vs counts of other years) is higher than the median of the calculated mean absolute percentage change numbers.
#'   \item \strong{getInterestingMonthlyData(threshold = 100, min_score = 4.0, period_threshold = 200, as_monthly_ts = T)} returns the union of unique CWEs found by methods \code{getChanging}, \code{getMostCritical} and \code{getMostInstances}. Output is monthly time series with mean monthly CVSS scores when \code{as_monthly_ts = T}, otherwise a \code{character} vector of CWE IDs.
#'   \item \strong{getMonthlyData(desired_cwes)} returns monthly time series with mean monthly CVSS scores of all available CWEs when \code{desired_cwes} is missing, otherwise only for those specified in the \code{character} vector \code{desired_cwes}
#' }
#' }
#'
#' @importFrom R6 R6Class
#' @name CWE
#' @export
#'
NULL
CWE <- R6::R6Class(
  "CWE",
  public = list(
    getStartYear = function(){
      private$start_year
    },
    getEndYear = function(plus_one = F){
      ifelse(plus_one, private$end_year + 1, private$end_year)
    },
    getStartMonth = function(as_number = F){
      if (as_number) {
        private$start_month
      } else {
        private$monthToChar(private$start_month)
      }
    },
    getEndMonth = function(as_number = F, plus_one = F){
      if (as_number) {
        ifelse(plus_one, private$end_month + 1, private$end_month)
      } else {
        ifelse(plus_one,
               private$monthToChar(private$end_month + 1),
               private$monthToChar(private$end_month))
      }
    },
    getBaseData = function(){
      private$base_data
    },
    getTimeSeriesData = function(){
      private$time_series_data
    },
    setStartYear = function(year){
      private$start_year <- year
    },
    setEndYear = function(year){
      private$end_year <- year
    },
    setStartMonth = function(smonth){
      private$start_month <- private$validateMonth(smonth)
    },
    setEndMonth = function(emonth){
      private$end_month <- private$validateMonth(emonth)
    },
    setBaseData = function(files){
      if (missing(files)) {
        data <- nvdr::nvd
      } else {
        data <- get_nvd_entries(files)
      }
      private$base_data <- data
    },
    setTimeSeriesData = function(ts_data){
      stopifnot(is.ts(ts_data))
      private$time_series_data <- ts_data
    },
    getAnalysisData = function() {
      data <- private$getTypeAndScore()
      vulns <- private$discernTypes(data)
      private$separateAndMerge(vulns[[1]], vulns[[2]])
    },
    getMostInstances = function(threshold = 100){
      cwe <- count <- NULL
      cwe_count_by_year <- private$getTimeGroupedData()
      any_year_has_n <-
        plyr::ddply(
          cwe_count_by_year,
          plyr::.(cwe), plyr::here(plyr::summarise),
          any_over_n = any(count >= threshold), sum = sum(count))
      plyr::arrange(
        any_year_has_n[which(any_year_has_n$any_over_n == TRUE),
                       c("cwe", "sum")], -sum)
    },
    getMostCritical = function(min_score = 4.0){
      cwe <- avg_cvss <- cvss <- NULL
      vulns_by_year <- private$getTimeGroupedData()
      year_range <- max(vulns_by_year[, private$published]) -
        min(vulns_by_year[, private$published]) + 1
      year_avg_cvss <- plyr::ddply(vulns_by_year,
                                   plyr::.(cwe), plyr::here(plyr::summarise),
                                   cvss = round(sum(avg_cvss) / year_range, 1))
      plyr::arrange(year_avg_cvss[year_avg_cvss$cvss > min_score, ], -cvss)
    },
    getChanging = function(period_threshold = 200){
      avg_change <- cwe <- avg_cvss <- cvss <- NULL
      cwe_instance_counts <- self$getMostInstances(threshold = 0)
      cwes_above_period_threshold <-
        cwe_instance_counts[cwe_instance_counts$sum >= period_threshold, "cwe"]
      vulns_by_year <- private$getTimeGroupedData()[, private$pub_cwe_count]
      overall_first_year <- min(vulns_by_year$published)
      overall_last_year <- max(vulns_by_year$published)
      if (overall_first_year == overall_last_year) {
        data.table::data.table(cwe = cwes_above_period_threshold,
                               avg_change = 0.00)
      } else {
        vulns_by_year_all <-
          private$fillMissingYearsWithZero(list(overall_first_year,
                                                overall_last_year),
                                           vulns_by_year,
                                           cwes_above_period_threshold)
        vulns_mean_change <-
          private$findYearlyMeanChange(cwes_above_period_threshold,
                                  vulns_by_year_all, overall_last_year)
        vulns_median <- stats::median(vulns_mean_change$avg_change)
        plyr::arrange(
          vulns_mean_change[vulns_mean_change$avg_change >= vulns_median, ],
          -avg_change)
        }
      },
    getInterestingMonthlyData = function(threshold = 100, min_score = 4.0,
                                 period_threshold = 200, as_monthly_ts = T){
      vulns_most_instances <- self$getMostInstances(threshold = threshold)
      vulns_most_impact <- self$getMostCritical(min_score = min_score)
      vulns_most_change <-
        self$getChanging(period_threshold = period_threshold)
      interesing_cwes <- unique(c(vulns_most_instances$cwe,
                                  vulns_most_impact$cwe,
                                  vulns_most_change$cwe))
      if (as_monthly_ts) {
        vulns_by_month <- private$getTimeGroupedData(monthly = T)
        vulns_by_month <- private$fillMissingMonthsWithZero(vulns_by_month,
                                                            interesing_cwes)
        private$intoTimeSeries(interesing_cwes, vulns_by_month)
      } else {
        interesing_cwes
      }
    },
    getMonthlyData = function(desired_cwes){
        vulns_by_month <- private$getTimeGroupedData(monthly = T)
        if (missing(desired_cwes)) {
          desired_cwes <- unique(vulns_by_month$cwe)
        }
        vulns_by_month <- private$fillMissingMonthsWithZero(vulns_by_month,
                                                            desired_cwes)
        private$intoTimeSeries(desired_cwes, vulns_by_month)
    }
  ),

  private = list(
    start_year = 2011,
    end_year = 2016,
    start_month = 1,
    end_month = 12,
    base_data = NA,
    pub_cwe_cvss = c("published", "cwe", "cvss_score"),
    pub_cwe_count =  c("published", "cwe", "count"),
    published = "published",
    time_series_data = NA,

    validateMonth = function(m) {
      if (m >= 1 & m <= 12) {
        m
      } else {
        stop("Please check your input.")
      }
    },
    monthToChar = function(m){
      if (m <= 9) {
        paste0(0, m)
      } else {
        paste0(m)
      }
    },
    discernTypes = function(data){
      mutli_cwe_condition <- grepl("|", data$cwe, fixed = TRUE)
      one_type_vulns <- data[!mutli_cwe_condition, ]
      multi_type_vulns <- data[mutli_cwe_condition, ]
      list(one_type_vulns, multi_type_vulns)
    },
    getTypeAndScore = function() {
      if (self$getStartYear() <= self$getEndYear()) {
        entries <- stats::na.omit(self$getBaseData()[, private$pub_cwe_cvss])
        data.table::set(entries, j = private$published,
                        value = as.Date(entries$published))
        entries[which(entries$published < private$getEndDate() &
                          entries$published >= private$getStartDate()), ]
      } else {
        stop("Please check your start and end numbers.")
      }
    },
    separateAndMerge = function(one_type_vulns, multi_type_vulns) {
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
    },
    getEndDate = function(straightforward = F){
      if (straightforward) {
        as_char <- paste0(self$getEndYear(), "-", self$getEndMonth(), "-01")
      } else {
        end_month_as_number <- self$getEndMonth(as_number = T)
        if (end_month_as_number == 12) {
          as_char <- paste0(self$getEndYear(plus_one = T), "-01", "-01")
          } else {
            as_char <- paste0(self$getEndYear(), "-",
                              self$getEndMonth(plus_one = T), "-01")
          }
      }
      as.Date(as_char)
    },
    getStartDate = function(){
      as_char <- paste0(self$getStartYear(), "-", self$getStartMonth(), "-01")
      as.Date(as_char)
    },
    dateFormatter = function(dates, monthly){
      if (monthly) {
        zoo::as.yearmon(dates)
      } else {
        as.numeric(format(zoo::as.yearmon(dates), "%Y"))
      }
    },
    getTimeGroupedData = function(monthly = F){
      cwe <- cvss_score <- published <- count <- cvss_sum <- NULL
      plyr::ddply(
        self$getAnalysisData(),
        plyr::.(published = private$dateFormatter(published, monthly), cwe),
        plyr::here(plyr::summarise),
        cvss_sum = sum(as.numeric(cvss_score)), count = length(published),
        avg_cvss = ifelse(count != 0, round(cvss_sum / count, 1), 0)
      )
    },
    fillMissingMonthsWithZero = function(vulns_by_month, selected_vulns_cwes) {
      published <- NULL
      months_no <- (self$getEndYear() - self$getStartYear() + 1) * 12
      full_dates <- zoo::as.yearmon(
        seq(private$getStartDate(), private$getEndDate(straightforward = T),
             by = "month"))
      missing <-
        data.table::rbindlist(lapply(selected_vulns_cwes, function(cwe) {
        if (!nrow(vulns_by_month[vulns_by_month$cwe == cwe, ]) == months_no) {
          missing_months <-
            zoo::as.yearmon(
              setdiff(full_dates,
                      vulns_by_month[vulns_by_month$cwe == cwe,
                                     private$published]))
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
    },
    fillMissingYearsWithZero = function(start_end, vulns_by_year,
                                            selected_vulns_cwes){
      published <- NULL
      missing <-
        data.table::rbindlist(lapply(selected_vulns_cwes, function(cwe) {
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
      plyr::arrange(data.table::rbindlist(list(vulns_by_year, missing)),
                    published)
    },
    findYearlyMeanChange =
      function(selected_vulns_cwes, vulns_by_year, last_year){
        data.table::rbindlist(lapply(selected_vulns_cwes, function(cwe_name){
          cwe_specific_counts <-
            vulns_by_year[vulns_by_year$cwe == cwe_name, ]
          last_year_count <-
            cwe_specific_counts[cwe_specific_counts$published == last_year,
                                "count"]
          other_year_counts <-
            cwe_specific_counts[cwe_specific_counts$published != last_year,
                                "count"]
          new_amount <- last_year_count
          base_amount <- other_year_counts
          percent_change <-
            ifelse(base_amount == 0, 0,
                   abs( (new_amount - base_amount) / base_amount))
          list(cwe = cwe_name, avg_change = round(mean(percent_change), 2))
        }
        ))
      },
    intoTimeSeries = function(chosen_vulns, vulns_by_month) {
        published <- NULL
        vulns_by_month <- plyr::arrange(vulns_by_month, published)
        vulns_to_ts <-
          lapply(chosen_vulns,
                 function(cwe_name) {
                   vulns_by_month[vulns_by_month$cwe == cwe_name, c("avg_cvss")]
                 }
          )
        names(vulns_to_ts) <- chosen_vulns
        stats::ts(data.table::setDT(vulns_to_ts),
                  frequency = 12,
                  start = c(self$getStartYear(),
                            self$getStartMonth(as_number = T)))
      }
  )
)
