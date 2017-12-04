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
    areResidualsNotRandom = function(){
      private$residuals_not_random
    },
    areResidualsNotNormal = function(){
      private$residuals_not_normal
    },
    isPiIgnored = function(){
      private$pi_ignored
    },
    isBoxCoxApplied = function(){
      private$box_cox_applied
    },
    isBootstrapNotUsed = function(){
      private$bootstrap_not_used
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
    setBoxCoxApplied = function(logical_value){
      private$box_cox_applied <- logical_value
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
      private$residuals_not_random <- (p_value <= 0.05)
    },
    testResidualsRandomnessBreusch = function(residuals, df, fitted_model){
      lag <- private$residuals_lag(df, residuals)
      p_value <- lmtest::bgtest(fitted_model, order = lag)$p.value
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
       if (self$areResidualsNotRandom() |
           (self$areResidualsNotNormal() & self$isBootstrapNotUsed()) |
           self$isPiIgnored()) {
         ylab <- ggplot2::ylab(paste("!", cwe_name, "CVSS"))
         ggplot2::autoplot(future, PI = T) + ylab + actual_line + no_guide
       } else {
         ylab <- ggplot2::ylab(paste(cwe_name, "CVSS"))
         ggplot2::autoplot(future, PI = T) + ylab + actual_line + no_guide
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
    box_cox_applied = F,

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
      min(max(df + 3, ifelse(freq > 1, 2 * freq, 10)),
          length(residuals) - 1)
    }
  )
)
