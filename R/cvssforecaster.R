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
    getBenchmark = function(cwe_name){
      if (missing(cwe_name)) {
        private$benchmark
      } else {
        private$getModelByCWE(private$benchmark, cwe_name)
      }
    },
    getARIMA = function(cwe_name){
      if (missing(cwe_name)) {
        private$arima
      } else {
        private$getModelByCWE(private$arima, cwe_name)
      }
    },
    getETS = function(cwe_name){
      if (missing(cwe_name)) {
        private$ets
      } else {
        private$getModelByCWE(private$ets, cwe_name)
      }
    },
    getTSLinear = function(cwe_name){
      if (missing(cwe_name)) {
        private$tslinear
      } else {
        private$getModelByCWE(private$tslinear, cwe_name)
      }
    },
    getNNAR = function(cwe_name){
      if (missing(cwe_name)) {
        private$nnar
      } else {
        private$getModelByCWE(private$nnar, cwe_name)
      }
    },
    getARFIMA = function(cwe_name){
      if (missing(cwe_name)) {
        private$arfima
      } else {
        private$getModelByCWE(private$arfima, cwe_name)
      }
    },
    getBaggedETS = function(cwe_name){
      if (missing(cwe_name)) {
        private$bagged_ets
      } else {
        private$getModelByCWE(private$bagged_ets, cwe_name)
      }
    },
    getTBATS = function(cwe_name){
      if (missing(cwe_name)) {
        private$tbats
      } else {
        private$getModelByCWE(private$tbats, cwe_name)
      }
    },
    getStructTS = function(cwe_name){
      if (missing(cwe_name)) {
        private$struct_ts
      } else {
        private$getModelByCWE(private$struct_ts, cwe_name)
      }
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
    setCWEs = function(cwe_input){
      private$start_year <- cwe_input$getStartYear()
      private$end_year <- cwe_input$getEndYear()
      if (private$end_year - private$start_year < 2) {
        stop("Please pick data with two years of yearnumber difference.")
      }
      ts_data <- cwe_input$getTimeSeriesData()
      private$cwes <- colnames(ts_data)
      private$cwes_ts <- ts_data
      private$end_month <- cwe_input$getEndMonth(as_number = T)
    }
    ),

  private = list(
    cwes = NA,
    cwes_ts = NA,
    start_year = NA,
    end_year = NA,
    end_month = NA,
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
        m <- m_class$new(cwe_name,
                         stats::as.ts(cwes_monthly_ts[, cwe_name]),
                         private$start_year, private$end_year,
                         private$end_month)
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
    },
    getModelByCWE = function(model_list, cwe_name){
      for (model in model_list) {
        if (model$getCWE() == cwe_name) {
            return(model)
        }
      }
    }
    )
  )
