#' Class NVDModel
#'
#' Manages forecasts of one CWE.
#'
#' @section Direct Subclasses:{
#' \itemize{
#' \item \link{FitModel},
#' \item \link{FcastModel}
#' }
#' }
#'
#' @section Private attributes:{
#' \itemize{
#'   \item \strong{cwe} CWE ID (default NA)
#'   \item \strong{start_year} the first year of a time period (default NA)
#'   \item \strong{end_year} the last year of a time period (default NA)
#'   \item \strong{train} the training set (default NA)
#'   \item \strong{test} the test set (default NA)
#'   \item \strong{fcast} the forecasts as a "\code{forecast}" object (default NA)
#'   \item \strong{assessment} the MAE, RMSE, MAPE and MASE in \code{numeric} form (default NA)
#'   \item \strong{fcast_period} the number of months necessary to forecast (default NA)
#'   \item \strong{residuals_not_random} the logical value indicating whether the models' residuals are not uncorrelated; cannot be used in the cases of \link{ARFIMAModel}, \link{BaggedETSModel}, \link{StructTSModel} and \link{NNARModel} for which ACF plots must be used instead (default F)
#'   \item \strong{residuals_not_normal} the logical value indicating whether the models' residuals are not normally distributed (default F)
#'   \item \strong{bootstrap_not_used} the logical value indicating whether the forecast intervals were not generated from bootstrapped residuals (default T)
#'   \item \strong{pi_ignored} the logical value; when F, then the forecast intervals can be taken seriously, otherwise the model's residuals must be checked (default F)
#'   \item \strong{significance_level} the significance level used in the Shapiro-Wilk, Ljung-Box and Breusch-Godfrey hypothesis cheking (default 0.05)
#'   \item \strong{measures = c("MAE", "RMSE", "MAPE", "MASE")} a \code{character} vector attribute used in the source code
#'   \item \strong{tset_char = "Test set"} a \code{character} attribute used in the source code (default F)
#' }
#' }
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{new(cwe, cwe_ts, start_year, end_year, end_month)} creates new NVD object
#'   \item \strong{getStartYear()} returns private attribute \code{start_year}
#'   \item \strong{getEndYear()} returns private attribute \code{end_year}
#'   \item \strong{getTrainingSet()} returns private attribute \code{train}
#'   \item \strong{getTestSet()} returns private attribute \code{test}
#'   \item \strong{getFcasted()} returns private attribute \code{fcast}
#'   \item \strong{getMethod()} returns attribute \code{method} of private attribute \code{fcast}
#'   \item \strong{getAssessment()} returns private attribute \code{assessment}
#'   \item \strong{getFcastPeriod()} returns private attribute \code{fcast_period}
#'   \item \strong{getTSetChar()} returns private attribute \code{tset_char}
#'   \item \strong{getResiduals(model)} returns residuals from \code{model} processed by \code{zoo::na.approx}; \code{model} is obtained from \link{FitModel}'s method \code{getFitted} or \link{NVDModel}'s method \code{getFcasted}
#'   \item \strong{getMergedTrainTestSet()} returns a \code{stats::ts} object after merging both the training set and the test set
#'   \item \strong{getSignificanceLevel()} returns private attribute \code{significance_level}
#'   \item \strong{areResidualsNotRandom()} returns private attribute \code{residuals_not_random}
#'   \item \strong{areResidualsNotNormal()} returns private attribute \code{residuals_not_normal}
#'   \item \strong{isPiIgnored()} returns private attribute \code{pi_ignored}
#'   \item \strong{isBootstrapNotUsed()} returns private attribute \code{bootstrap_not_used}
#'   \item \strong{setSignificanceLevel(significance)} sets \code{significance} as the value of private attribute \code{significance_level}
#'   \item \strong{setResidualsNotRandom(testresult)} sets \code{testresult} as the value of private attribute \code{residuals_not_random}
#'   \item \strong{setResidualsNotNormal(testresult)} sets \code{testresult} as the value of private attribute \code{residuals_not_normal}
#'   \item \strong{setBootstrapNotUsed(logical_value)} sets \code{logical_value} as the value of private attribute \code{bootstrap_not_used}
#'   \item \strong{setPiIgnored(logical_value)} sets \code{logical_value} as the value of private attribute \code{pi_ignored}
#'   \item \strong{setFcasted(fcast_result)} sets \code{fcast_result} as the value of private attribute \code{fcast}
#'   \item \strong{setFcastPeriod(period)} sets \code{period} as the value of private attribute \code{fcast_period}
#'   \item \strong{assessModel()} calculates and sets MAE, RMSE, MAPE, MASE values as the value of private attribute \code{assessment}
#'   \item \strong{testResidualsRandomnessBox(residuals, df)} uses residuals and degrees of freedom arguments and performs the Ljung-Box test; returns whether the obtained p-value is less or equal than the significance level
#'   \item \strong{testResidualsRandomnessBreusch(residuals, df, fitted_model)} uses residuals, degrees of freedom and fitted model arguments and performs the Breusch-Godfrey test; returns whether the obtained p-value is less or equal than the significance level
#'   \item \strong{testResidualsNormality(residuals)} uses residuals arguments and performs the Shapiro-Wilk test; returns whether the obtained p-value is less or equal than the significance level
#'   \item \strong{getPlot()} returns a plot of forecast results and actual values as a red line when the forecasting results are available in the private attribute \code{fcast}; adds an exclamation mark to y-axis label when \code{self$areResidualsNotRandom() | (self$areResidualsNotNormal() & self$isBootstrapNotUsed()) | self$isPiIgnored()}
#'   \item \strong{compareCombinedAccuracy(fcast1, fcast2)} takes two collections of forecasts and returns a logical value indicating whether the first collection of forecasted values has bigger amount of smaller MAE, RMSE, MAPE and MASE values than the other collection of forecasted values
#'   \item \strong{findLambda(training_set)} returns the lambda found by \href{http://pkg.robjhyndman.com/forecast/reference/BoxCox.lambda.html}{forecast::BoxCox.lambda} based on the training set
#' }
#' }
#'
#' @section Note: {
#' This class's private method `residuals_lag` is using GPL-3 licensed code from (R package "forecast"
#' by Rob Hyndman, Mitchell O'Hara-Wild, Christoph Bergmeir, Slava Razbash
#' and Earo Wang)
#' \href{https://github.com/robjhyndman/forecast/blob/c87f33/R/checkresiduals.R}{https://github.com/robjhyndman/forecast/blob/c87f33/R/checkresiduals.R}
#' from lines 122 and 123 to calculate the lag parameter.
#' }
#'
#' @importFrom R6 R6Class
#' @keywords internal
#' @name NVDModel
#' @export
#'
NULL
NVDModel <- R6::R6Class(
  "NVDModel",
  public = list(
    initialize = function(cwe, cwe_ts, start_year, end_year, end_month) {
      private$cwe <- cwe
      private$start_year <- start_year
      private$end_year <- end_year
      private$train <- stats::window(cwe_ts, end = c(private$end_year - 1, 12))
      private$test <- stats::window(cwe_ts,
                                    start = c(private$end_year, 1),
                                    end = c(private$end_year, end_month))
      self$setFcastPeriod(end_month)
    },
    getCWE = function(){
      private$cwe
    },
    getStartYear = function(){
      private$start_year
    },
    getEndYear = function(){
      private$end_year
    },
    getTrainingSet = function(){
      private$train
    },
    getTestSet = function(){
      private$test
    },
    getFcasted = function(){
      private$fcast
    },
    getMethod = function(){
      self$getFcasted()$method
    },
    getAssessment = function(){
      private$assessment
    },
    getFcastPeriod = function(){
      private$fcast_period
    },
    getTSetChar = function(){
      private$tset_char
    },
    getResiduals = function(model){
      zoo::na.approx(stats::residuals(model))
    },
    getMergedTrainTestSet = function(){
      train <- self$getTrainingSet()
      ts(c(train, self$getTestSet()), start = start(train),
         frequency = frequency(train))
    },
    getSignificanceLevel = function(){
      private$significance_level
    },
    areResidualsNotRandom = function(){
      private$residuals_not_random
    },
    areResidualsNotNormal = function(){
      private$residuals_not_normal
    },
    isPiIgnored = function(){
      private$pi_ignored
    },
    isBootstrapNotUsed = function(){
      private$bootstrap_not_used
    },
    setSignificanceLevel = function(significance){
      private$significance_level <- significance
    },
    setResidualsNotRandom = function(testresult){
      private$residuals_not_random <- testresult
    },
    setResidualsNotNormal = function(testresult){
      private$residuals_not_normal <- testresult
    },
    setBootstrapNotUsed = function(logical_value){
      private$bootstrap_not_used <- logical_value
    },
    setPiIgnored = function(logical_value){
      private$pi_ignored <- logical_value
    },
    setFcasted = function(fcast_result){
      private$fcast <- fcast_result
    },
    setFcastPeriod = function(period){
      private$fcast_period <- period
    },
    assessModel = function(){
      private$assessment <-
        forecast::accuracy(self$getFcasted(),
                           self$getTestSet())[self$getTSetChar(),
                                              private$measures]
    },
    testResidualsRandomnessBox = function(residuals, df){
      p_value <- stats::Box.test(
        residuals, fitdf = df, lag = private$residuals_lag(df, residuals),
        type = "Ljung-Box")$p.value
      p_value <= self$getSignificanceLevel()
    },
    testResidualsRandomnessBreusch = function(residuals, df, fitted_model){
      lag <- private$residuals_lag(df, residuals)
      p_value <- lmtest::bgtest(fitted_model, order = lag)$p.value
      p_value <= self$getSignificanceLevel()
    },
    testResidualsNormality = function(residuals){
      p_value <- stats::shapiro.test(residuals)$p.value
      p_value <= self$getSignificanceLevel()
    },
   getPlot = function(){
       cwe_name <- self$getCWE()
       future <- self$getFcasted()
       actual <- self$getTestSet()
       no_guide <- ggplot2::guides(colour = ggplot2::guide_legend(title = ""))
       actual_line <- forecast::autolayer(actual, series = "Actual")
       if (self$areResidualsNotRandom() |
           (self$areResidualsNotNormal() & self$isBootstrapNotUsed()) |
           self$isPiIgnored()) {
         ylab <- ggplot2::ylab(paste("!", cwe_name, "CVSS"))
         ggplot2::autoplot(future, PI = T) + ylab + actual_line + no_guide +
           ggplot2::scale_y_continuous(limits = private$plot_limits())
       } else {
         ylab <- ggplot2::ylab(paste(cwe_name, "CVSS"))
         ggplot2::autoplot(future, PI = T) + ylab + actual_line + no_guide +
           ggplot2::scale_y_continuous(limits = private$plot_limits())
       }
     },
   compareCombinedAccuracy = function(fcast1, fcast2){
     test <- self$getTestSet()
     accuracy1 <- forecast::accuracy(fcast1, test)[self$getTSetChar(),
                                                   private$measures]
     accuracy2 <- forecast::accuracy(fcast2, test)[self$getTSetChar(),
                                                   private$measures]
     metrics <- cbind(accuracy1, accuracy2)
     ifelse(nvdr::findBestColumn(metrics) == "accuracy1", T, F)
   },
   findLambda = function(training_set){
     tryCatch(forecast::BoxCox.lambda(training_set), warning = function(w) NULL)
   }
  ),

  private = list(
    cwe = NA,
    start_year = NA,
    end_year = NA,
    train = NA,
    test = NA,
    fcast = NA,
    assessment = NA,
    fcast_period = NA,
    residuals_not_random = F,
    residuals_not_normal = F,
    bootstrap_not_used = T,
    pi_ignored = F,
    measures = c("MAE", "RMSE", "MAPE", "MASE"),
    tset_char = "Test set",
    significance_level = 0.05,

    plot_limits = function() {
      future <- self$getFcasted()
      if (length(future$upper) > 0) {
        c(min(future$lower, self$getTestSet(), self$getTrainingSet()),
          max(future$upper,  self$getTestSet(), self$getTrainingSet()))
      } else {
        c(min(future$mean, self$getTestSet(), self$getTrainingSet()),
          max(future$mean, self$getTestSet(), self$getTrainingSet()))
      }
    },

    residuals_lag = function(df, residuals){
      freq <- frequency(residuals)
      min(max(df + 3, ifelse(freq > 1, 2 * freq, 10)),
          length(residuals) - 1)
    }
  )
)
