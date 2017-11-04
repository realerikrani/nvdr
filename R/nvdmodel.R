NVDModel <- R6::R6Class(
  "NVDModel",
  public = list(
    initialize = function(cwe, cwe_ts) {
      stopifnot(is.character(cwe))
      stopifnot(is.ts(cwe_ts))
      private$cwe <- cwe
      start_year <- as.integer(min(zoo::as.yearmon(stats::time(cwe_ts))))
      end_year <- as.integer(max(zoo::as.yearmon(stats::time(cwe_ts))))
      if (end_year - start_year < 2) {
        stop("Please pick data covering at least two years.")
      }
      private$start_year <- start_year
      private$end_year <- end_year
      private$train <- stats::window(cwe_ts, end = c(private$end_year - 1,
                                                  private$fcast_period))
      private$test <- stats::window(cwe_ts, start = c(private$end_year, 1))
    },
    getCWE = function(){
      private$cwe
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
    areResidualsNotRandom = function(){
      private$residuals_not_random
    },
    areResidualsNotNormal = function(){
      private$residuals_not_normal
    },
    isPiIgnored = function(){
      private$pi_ignored
    },
    setResidualsNotRandom = function(testresult){
      private$residuals_not_random <- testresult
    },
    setResidualsNotNormal = function(testresult){
      private$residuals_not_normal <- testresult
    },
    setPiIgnored = function(logical_value){
      private$pi_ignored <- logical_value
    },
    setFcasted = function(fcast_result){
      private$fcast <- fcast_result
    },
    assessModel = function(){
      tset_char <- "Test set"
      private$assessment <-
        forecast::accuracy(self$getFcasted(),
                           self$getTestSet())[tset_char, private$measures]

      },
    testResidualsRandomnessBox = function(residuals, df){
      p_value <- stats::Box.test(
        residuals, fitdf = df, lag = private$residuals_lag(df, residuals),
        type = "Ljung-Box")$p.value
      private$residuals_not_random <- (p_value <= 0.05)
    },
    testResidualsRandomnessBreusch = function(residuals, df, fitted_model){
      lag <- private$residuals_lag(df, residuals)
      p_value <- lmtest::bgtest(fitted_model,order = lag)$p.value
      private$residuals_not_random <- (p_value <= 0.05)
    },
    testResidualsNormality = function(residuals){
      p_value <- stats::shapiro.test(residuals)$p.value
      private$residuals_not_normal <- (p_value <= 0.05)
    },
   getPlot = function(){
       cwe_name <- self$getCWE()
       future <- self$getFcasted()
       actual <- self$getTestSet()
       no_guide <- ggplot2::guides(colour = ggplot2::guide_legend(title = ""))
       actual_line <- forecast::autolayer(actual, series = "Actual")
       if (self$areResidualsNotRandom() | self$areResidualsNotNormal() |
           self$isPiIgnored()) {
         ylab <- ggplot2::ylab(paste("!", cwe_name, "CVSS"))
         ggplot2::autoplot(future, PI = T) + ylab + actual_line + no_guide
       } else {
         ylab <- ggplot2::ylab(paste(cwe_name, "CVSS"))
         ggplot2::autoplot(future, PI = T) + ylab + actual_line + no_guide
       }
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
    fcast_period = 12,
    residuals_not_random = FALSE,
    residuals_not_normal = FALSE,
    pi_ignored = FALSE,
    measures = c("MAE", "RMSE", "MAPE", "MASE"),

    #' Lag finder
    #'
    #' Function, using GPL-3 licensed code from (R package "forecast"
    #' by Rob Hyndman, Mitchell O'Hara-Wild, Christoph Bergmeir, Slava Razbash
    #' and Earo Wang)
    #' https://github.com/robjhyndman/forecast/blob/c87f33/R/checkresiduals.R
    #' from lines 122 and 123 to calculate the lag parameter.
    #' @keywords internal
    residuals_lag = function(df, residuals){
      freq <- frequency(residuals)
      min(max(df + 3, ifelse(freq > 1, 2*freq, 10)),
          length(residuals) - 1)
    }
  )
)
