#' CVSS Forecaster
#'
#' Forecasts monthly mean CVSS scores of subsets of CWE vulnerabilities.
#'
#' @section Usage:
#' \preformatted{
#' ## Create new forecasting object.
#' cf <- CVSSForecaster$new()
#' ## Find interesting CWE vulnerability categories between years 2011 and 2016.
#' cf$setCWENamesAndSeries()
#' ## Set models (creates the forecasts models for the selected CWEs' time
#' series and measures the accuracy).
#' cf$setARIMA()
#' cf$setETS()
#' ## Get forecast plots.
#' cf$getPlots(cf$getARIMA())
#' cf$getPlots(cf$getETS())
#' ## Get ARIMA model's AICc, AIC and BIC.
#' cf$getInformationCriterions(cf$getARIMA())
#' ## Get forecast accuracy measures.
#' cf$getAssessments(cf$getARIMA())
#' }
#'
#' @section Details:
#' \code{$new()} creates new CVSSForecaster object
#'
#' @importFrom R6 R6Class
#' @name CVSSForecaster
#' @examples
#' \dontrun{
#' ## Create new forecasting object.
#' cf <- CVSSForecaster$new()
#' ## Find interesting CWE vulnerability categories between years 2011 and 2016.
#' cf$setCWENamesAndSeries()
#' ## Set models (creates the forecasts models for the selected CWEs' time
#' series and measures the accuracy).
#' cf$setARIMA()
#' cf$setETS()
#' ## Get forecast plots.
#' cf$getPlots(cf$getARIMA())
#' cf$getPlots(cf$getETS())
#' ## Get ARIMA model's AICc, AIC and BIC.
#' cf$getInformationCriterions(cf$getARIMA())
#' ## Get forecast accuracy measures.
#' cf$getAssessments(cf$getARIMA())
#' }
#'
NULL

#' @export
CVSSForecaster <- R6::R6Class(
  "CVSSForecaster",
  public = list(
    getCWENames = function(){
      private$cwes
    },
    getSeries = function(){
      private$cwes_ts
    },
    getBenchmark = function(){
      private$benchmark
    },
    getARIMA = function(){
      private$arima
    },
    getETS = function(){
      private$ets
    },
    getTSLinear = function(){
      private$tslinear
    },
    getNNAR = function(){
      private$nnar
    },
    getARFIMA = function(){
      private$arfima
    },
    getBaggedETS = function(){
      private$bagged_ets
    },
    getTBATS = function(){
      private$tbats
    },
    getStructTS = function(){
      private$struct_ts
    },
    getPlots = function(model_list, row_no = 5, col_no = 2){
      private$plotter(model_list, row_no = row_no, col_no = col_no)
    },
    getAssessments = function(model_list){
      private$measurer(model_list)
    },
    getInformationCriterions = function(model_list){
      private$ic(model_list)
    },
    setBenchmark = function(){
      private$benchmark <- private$model(self$getSeries(), BenchmarkModel)
    },
    setARIMA = function(stepwise = T, approximation = T){
      private$arima <- private$model(self$getSeries(), ARIMAModel,
                                     stepwise = stepwise,
                                     approximation = approximation)
    },
    setETS = function(){
      private$ets <- private$model(self$getSeries(), ETSModel)
    },
    setTSLinear = function(){
      private$tslinear <- private$model(self$getSeries(), TSLinearModel)
    },
    setNNAR = function(pi_simulation = F){
      private$nnar <- private$model(self$getSeries(), NNARModel,
                                    pi_simulation = pi_simulation)
    },
    setARFIMA = function(){
      private$arfima <- private$model(self$getSeries(), ARFIMAModel)
    },
    setBaggedETS = function(){
      private$bagged_ets <- private$model(self$getSeries(), BaggedETSModel)
    },
    setTBATS = function(){
      private$tbats <- private$model(self$getSeries(), TBATSModel)
    },
    setStructTS = function(){
      private$struct_ts <- private$model(self$getSeries(), StructTSModel)
    },
    setCWENamesAndSeries = function(start_year=2011, end_year=2016,
                                    threshold = 100, min_score = 4.0,
                                    period_threshold = 200){
      cwes <- get_interesting_cwes(start_year, end_year, threshold, min_score,
                                   period_threshold)
      private$cwes <- colnames(cwes)
      private$cwes_ts <- cwes
    }
    ),

  private = list(
    cwes = NA,
    cwes_ts = NA,
    benchmark = NA,
    arima = NA,
    ets = NA,
    tslinear = NA,
    nnar = NA,
    arfima = NA,
    bagged_ets = NA,
    tbats = NA,
    struct_ts = NA,

    model = function(cwes_monthly_ts, m_class, ...){
      lapply(colnames(cwes_monthly_ts), function(cwe_name){
        m <- m_class$new(cwe_name, stats::as.ts(cwes_monthly_ts[, cwe_name]))
        m$buildModel(...)
        m$assessModel()
        m
      }
      )
    },
    plotter = function(model_list, row_no, col_no){
      gridExtra::marrangeGrob(
        lapply(c(1:length(model_list)), function(list_index){
          model_list[[list_index]]$getPlot()
          }
        ), nrow = row_no, ncol = col_no
      )
      },
    measurer = function(model_list){
      data.table::rbindlist(
        lapply(c(1:length(model_list)), function(list_index){
          m <- model_list[[list_index]]
          as.list(
            c(cwe = m$getCWE(), method = m$getMethod(),
              round(m$getAssessment(), 4))
          )
        })
      )
    },
    ic = function(model_list){
      data.table::rbindlist(
        lapply(c(1:length(model_list)), function(list_index){
          m <- model_list[[list_index]]
          as.list(
            c(cwe = m$getCWE(), aicc = round(m$getFitted()$aicc, 4),
              aic = round(m$getFitted()$aic, 4),
              bic = round(m$getFitted()$bic, 4))
          )
        })
      )
    }
    )
  )
