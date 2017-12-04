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
    getAllAssessments = function(){
      models <- private$getBuiltModels()
      cwe_names <- self$getCWENames()
      Reduce(function(a, b) merge(a, b, all = T),
             lapply(models, function(m){
               data.table::rbindlist(lapply(cwe_names, function(cwe_name){
                 specific_cwe_model <- private$getModelByCWE(m, cwe_name)
                 measures <- specific_cwe_model$getAssessment()
                 list(cwe = cwe_name, method = specific_cwe_model$getMethod(),
                      MAE = measures["MAE"],RMSE = measures["RMSE"],
                      MAPE = measures["MAPE"],MASE = measures["MASE"])
               }
               ))
             }
             ))
    },
    compareAssessments = function(model_list1, model_list2){
      merged <- merge(self$getAssessments(model_list1),
                      self$getAssessments(model_list2), by = "cwe")
      print(paste(class(model_list1[[1]])[[1]], "<=",
                  class(model_list2[[1]])[[1]]))
      data.table::rbindlist(apply(merged, 1, function(assessrow){
        cwe_name <- assessrow[1]
        method1 = assessrow[2]
        method2 = assessrow[7]
        mae1 <- as.numeric(assessrow[3])
        mae2 <- as.numeric(assessrow[8])
        mae_best <- ifelse(mae1 < mae2, method1, method2)
        rmse1 <- as.numeric(assessrow[4])
        rmse2 <- as.numeric(assessrow[9])
        rmse_best <- ifelse(rmse1 < rmse2, method1, method2)
        mape1 <- as.numeric(assessrow[5])
        mape2 <- as.numeric(assessrow[10])
        mape_best <- ifelse(mape1 < mape2, method1, method2)
        mase1 <- as.numeric(assessrow[6])
        mase2 <- as.numeric(assessrow[11])
        mase_best <- ifelse(mase1 < mase2, method1, method2)
        best_vector <- c(mae_best, rmse_best, mape_best, mase_best)
        best <- names(sort(summary(as.factor(best_vector)),
                           decreasing = T)[1])
        list(cwe = cwe_name,
             method = ifelse(best == method1, best, "X"),
             MAE = ifelse(mae1 < mae2, mae1, "X"),
             RMSE = ifelse(rmse1 < rmse2, rmse1, "X"),
             MAPE = ifelse(mape1 < mape2, mape1, "X"),
             MASE = ifelse(mase1 < mase2, mase1, "X"))
      }
      ))
    },
    getInformationCriterions = function(model_list){
      private$ic(model_list)
    },
    getBestModelList = function(){
      private$best_model_list
    },
    getBestAssessments = function(){
      assessments <- self$getAllAssessments()
      data.table::rbindlist(lapply(self$getCWENames(), function(cwe_name){
        selection <- assessments[assessments$cwe == cwe_name, ]
        best <- names(sort(summary(
          as.factor(c(selection[which.min(selection$MAE), ]$method,
                      selection[which.min(selection$RMSE), ]$method,
                      selection[which.min(selection$MAPE), ]$method,
                      selection[which.min(selection$MASE), ]$method))),
          decreasing = T)[1])
        list(cwe = cwe_name, method = best,
             MAE = selection[selection$method == best, ]$MAE,
             RMSE = selection[selection$method == best, ]$RMSE,
             MAPE = selection[selection$method == best, ]$MAPE,
             MASE = selection[selection$method == best, ]$MASE)
      }
      ))
    },
    useBest = function(fcast_period){
      best_assessments <- self$getBestAssessments()[,c("cwe","method")]
      apply(best_assessments, 1, function(cwe_and_method){
        cwe_name <- cwe_and_method[[1]]
        method_name <- cwe_and_method[[2]]
        result <- NULL
        name <- function(x) grepl(paste0("\\b",x,"\\b"), method_name,
                                        ignore.case = T)
        if (name("naive") | name("mean") | (name("random") & name("walk")
                                            & name("drift"))) {
          result <- self$getBenchmark(cwe_name)$useModel(fcast_period,
                                               residuals_check = F)
        } else if (name("linear") & name("regression")) {
          result <- self$getTSLinear(cwe_name)$useModel(fcast_period,
                                                        residuals_check = F)
        } else if (name("ets")) {
          result <- self$getETS(cwe_name)$useModel(fcast_period,
                                                   residuals_check = F)
        } else if (name("arima")) {
          result <- self$getARIMA(cwe_name)$useModel(fcast_period,
                                                     residuals_check = F)
        }
        list(cwe = cwe_name, future = result)
      }
      )
    },
    plotUseBest = function(bestInUse, row_no, col_no, compact = F){
         plots <- lapply(c(1:length(bestInUse)), function(list_index){
           ylabtitle <- ggplot2::ylab(bestInUse[[list_index]]$cwe)
           values <- bestInUse[[list_index]]$future
           if (compact) {
             ggplot2::autoplot(values) + ylabtitle +
               ggplot2::guides(fill = F) + ggplot2::ggtitle("")
             } else {
               ggplot2::autoplot(values) + ylabtitle
             }
           }
           )
         gridExtra::marrangeGrob(plots, nrow = row_no, ncol = col_no)
    },
    setBenchmark = function(destroy = F){
      if (destroy) {
        private$benchmark <- NA
      } else {
        private$benchmark <- private$model(self$getSeries(), BenchmarkModel)
      }
    },
    setARIMA = function(stepwise = T, approximation = T, destroy = F){
      if (destroy) {
        private$arima <- NA
      } else {
        private$arima <- private$model(self$getSeries(), ARIMAModel,
                                       stepwise = stepwise,
                                       approximation = approximation)
      }
    },
    setETS = function(destroy = F){
      if (destroy) {
        private$ets <- NA
      } else {
        private$ets <- private$model(self$getSeries(), ETSModel)
      }
    },
    setTSLinear = function(destroy = F){
      if (destroy) {
        private$tslinear <- NA
      } else {
        private$tslinear <- private$model(self$getSeries(), TSLinearModel)
      }
    },
    setNNAR = function(pi_simulation = F, destroy = F){
      if (destroy) {
        private$nnar <- NA
      } else {
        private$nnar <- private$model(self$getSeries(), NNARModel,
                                      pi_simulation = pi_simulation)
      }
    },
    setARFIMA = function(destroy = F){
      if (destroy) {
        private$arfima <- NA
      } else {
        private$arfima <- private$model(self$getSeries(), ARFIMAModel)
      }
    },
    setBaggedETS = function(destroy = F){
      if (destroy) {
        private$bagged_ets <- NA
      } else {
        private$bagged_ets <- private$model(self$getSeries(), BaggedETSModel)
      }
    },
    setTBATS = function(destroy = F){
      if (destroy) {
        private$tbats <- NA
      } else {
        private$tbats <- private$model(self$getSeries(), TBATSModel)
      }
    },
    setStructTS = function(destroy = F){
      if (destroy) {
        private$struct_ts <- NA
      } else {
        private$struct_ts <- private$model(self$getSeries(), StructTSModel)
      }
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
    },
    setBestModelList = function(){
      best_assessments <- self$getBestAssessments()[,c("cwe","method")]
      private$best_model_list <-
        apply(best_assessments, 1, function(cwe_and_method){
          cwe_name <- cwe_and_method[[1]]
          method_name <- cwe_and_method[[2]]
          result <- NULL
          name <- function(x) grepl(paste0("\\b",x,"\\b"), method_name,
                                    ignore.case = T)
          if (name("naive") | name("mean") | (name("random") & name("walk")
                                              & name("drift"))) {
            return(self$getBenchmark(cwe_name))
          } else if (name("linear") & name("regression")) {
            return(self$getTSLinear(cwe_name))
          } else if (name("ets")) {
            return(self$getETS(cwe_name))
          } else if (name("arima")) {
            return(self$getARIMA(cwe_name))
          }
        }
        )
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
    best_model_list = NA,

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
    },
    getBuiltModels = function(){
      models <- list(self$getBenchmark(), self$getARIMA(),  self$getETS(),
                     self$getTSLinear(), self$getNNAR(), self$getARFIMA(),
                     self$getBaggedETS(), self$getTBATS(), self$getStructTS())
      set_models <- list()
      for (index in c(1:length(models))) {
        if (length(models[index]) == 1) {
          if (!is.na(models[index])) {
            set_models <- append(set_models, models[index])
          }
        } else {
          set_models <- append(set_models, models[index])
        }
      }
      set_models
    }
    )
  )
