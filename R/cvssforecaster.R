#' Class CVSSForecaster
#'
#' Forecasts monthly mean CVSS scores of subsets of CWE vulnerabilities.
#'
#' @section Private attributes:{
#' All private attribute's have \code{NA} as default value.
#' \itemize{
#'   \item \strong{cwes} the \code{character} vector of available CWEs
#'   \item \strong{cwes_ts} monthly time series of mean monthly CVSS scores of the CWEs in private attribute \code{cwes}
#'   \item \strong{start_year} the first year of a time period
#'   \item \strong{end_year} the last year of a time period
#'   \item \strong{end_month} the last month of a time period
#'   \item \strong{benchmark} the attribute for holding a list of \link{BenchmarkModel} objects
#'   \item \strong{arima} the attribute for holding a list of \link{ARIMAModel} objects
#'   \item \strong{ets} the attribute for holding a list of \link{ETSModel} objects
#'   \item \strong{tslinear} the attribute for holding a list of \link{TSLinearModel} objects
#'   \item \strong{nnar} the attribute for holding a list of \link{NNARModel} objects
#'   \item \strong{arfima} the attribute for holding a list of \link{ARFIMAModel} objects
#'   \item \strong{bagged_ets} the attribute for holding a list of \link{BaggedETSModel} objects
#'   \item \strong{tbats} the attribute for holding a list of \link{TBATSModel} objects
#'   \item \strong{struct_ts} the attribute for holding a list of \link{StructTSModel} objects
#'   \item \strong{best_model_list} the attribute for holding a list of forecasting model objects which gave the best overall forecast accuracy results for specific CWEs
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{new()} creates new CVSSForecaster object
#'   \item \strong{getCWENames()} returns private attribute \code{cwes}
#'   \item \strong{getSeries()} returns private attribute \code{cwes_ts}
#'   \item \strong{getBenchmark(cwe_name)} returns private attribute \code{benchmark} when the argument \code{cwe_name} is missing; otherwise returns a \link{BenchmarkModel} object (from \code{benchmark}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getARIMA(cwe_name)} returns private attribute \code{arima} when the argument \code{cwe_name} is missing; otherwise returns a \link{ARIMAModel} object (from \code{arima}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getETS(cwe_name)} returns private attribute \code{ets} when the argument \code{cwe_name} is missing; otherwise returns a \link{ETSModel} object (from \code{ets}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getTSLinear(cwe_name)} returns private attribute \code{tslinear} when the argument \code{cwe_name} is missing; otherwise returns a \link{TSLinearModel} object (from \code{tslinear}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getNNAR(cwe_name)} returns private attribute \code{nnar} when the argument \code{cwe_name} is missing; otherwise returns a \link{NNARModel} object (from \code{nnar}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getARFIMA(cwe_name)} returns private attribute \code{arfima} when the argument \code{cwe_name} is missing; otherwise returns a \link{ARFIMAModel} object  (from \code{arfima}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getBaggedETS(cwe_name)} returns private attribute \code{bagged_ets} when the argument \code{cwe_name} is missing; otherwise returns a \link{BaggedETSModel} object (from \code{bagged_ets}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getTBATS(cwe_name)} returns private attribute \code{tbats} when the argument \code{cwe_name} is missing; otherwise returns a \link{TBATSModel} object (from \code{tbats}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getStructTS(cwe_name)} returns private attribute \code{struct_ts} when the argument \code{cwe_name} is missing; otherwise returns a \link{StructTSModel} object (from \code{struct_ts}) corresponding to \code{cwe_name} value (CWE ID)
#'   \item \strong{getPlots(model_list, row_no = 5, col_no = 2)} returns plots of objects in the \code{model_list} with \code{row_no} number of rows and \code{col_no} number of columns arranged by \link[gridExtra]{marrangeGrob}
#'   \item \strong{getAssessments(model_list)} returns a \link[data.table]{data.table} with columns \code{cwe}, \code{method}, \code{MAE}, \code{RMSE}, \code{MAPE} and \code{MASE} extracted from the list of models from \code{model_list}
#'   \item \strong{getAllAssessments()} returns a \link[data.table]{data.table} with columns \code{cwe}, \code{method}, \code{MAE}, \code{RMSE}, \code{MAPE} and \code{MASE} extracted from the models that the \code{CVSSForecaster} has built and stored in its private attributes.
#'   \item \strong{getBestModelList()} returns private attribute \code{best_model_list}
#'   \item \strong{getBestAssessments()} returns a \link[data.table]{data.table} with columns \code{cwe}, \code{method}, \code{MAE}, \code{RMSE}, \code{MAPE} and \code{MASE} extracted from a subset of models returned by \code{getAllAssessments()}; subset represents the models which have the biggest number of lowest MAE, RMSE, MAPE and MASE values for a given CWE
#'   \item \strong{useBest(fcast_period, arima_stepwise = F, arima_approx = F, nnar_pi_sim = T)} returns a list of lists, where each list has two elements: CWE ID and the forecasts \code{fcast_period}-step ahead into the future as a "\code{forecast}" object generated by the same type of model that generated previously the most accurate forecasts according to \link{CVSSForecaster}'s method \code{getBestAssessments}
#'   \item \strong{plotUseBest(bestInUse, row_no, col_no, compact = F, actual)} returns plots of objects in the \code{bestInUse} object previously generated by \link{CVSSForecaster}'s method \code{useBest} with \code{row_no} number of rows and \code{col_no} number of columns; argument \code{compact = T} removes details from plots; when argument \code{actual} is not missing, then the actual values are added as a red line to the plot (should be the time series output of \link{CWE} object's method \code{getTimeSeriesData})
#'   \item \strong{assessUseBest(bestInUse, actual)} returns a \link[data.table]{data.table} with columns \code{cwe}, \code{method}, \code{MAE}, \code{RMSE}, \code{MAPE} and \code{MASE} where the forecast accuracy numbers are calculated by using the forecasting output by \link{CVSSForecaster}'s method \code{useBest} and the time series of actual values in a form given by \link{CWE} object's method \code{getTimeSeriesData}); example: \link{CVSSForecaster}'s method \code{useBest} generated forecasts for 2018 based on the available data from 2013 to 2017, which was eventually checked when 2018 data became available
#'   \item \strong{setBenchmark(destroy = F)} builds and assesses \link{BenchmarkModel} and assigns the result to the private attribute \code{benchmark}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{benchmark}
#'   \item \strong{setARIMA(stepwise = T, approximation = T, destroy = F)} builds and assesses \link{ARIMAModel} and assigns the result to the private attribute \code{arima}; the arguments \code{stepwise} and \code{approximation} are passed eventually to \href{http://pkg.robjhyndman.com/forecast/reference/auto.arima.html}{forecast::auto.arima}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{arima}
#'   \item \strong{setETS(destroy = F)} builds and assesses \link{ETSModel} and assigns the result to the private attribute \code{ets}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{ets}
#'   \item \strong{setTSLinear(destroy = F)} builds and assesses \link{TSLinearModel} and assigns the result to the private attribute \code{tslinear}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{tslinear}
#'   \item \strong{setNNAR(pi_simulation = F, destroy = F)} builds and assesses \link{NNARModel} and assigns the result to the private attribute \code{nnar}; the value of the argument \code{pi_simulation} is passed eventually to \link[forecast]{nnetar} as the value of that method's argument \code{PI}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{nnar}
#'   \item \strong{setARFIMA(destroy = F)} builds and assesses \link{ARFIMAModel} and assigns the result to the private attribute \code{arfima}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{arfima}
#'   \item \strong{setBaggedETS(destroy = F)} builds and assesses \link{BaggedETSModel} and assigns the result to the private attribute \code{bagged_ets}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{bagged_ets}
#'   \item \strong{setTBATS(destroy = F)} builds and assesses \link{TBATSModel} and assigns the result to the private attribute \code{tbats}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{tbats}
#'   \item \strong{setStructTS(destroy = F)} builds and assesses \link{StructTSModel}  and assigns the result to the private attribute \code{tbats}; if \code{destroy = F} then sets \code{NA} as the value of private attribute \code{struct_ts}
#'   \item \strong{setCWEs(cwe_input)} extracts start year, end year, end month, time series data and CWE IDs from argument \code{cwe_input} of class \link{CWE}; gives values to private attributes \code{start_year}, \code{end_year}, \code{cwes}, \code{cwes_ts} and \code{end_month}
#'   \item \strong{setBestModelList()} finds the model objects corresponding to \code{CVSSForecaster}'s method \code{getBestAssessments} output and assings the list of these objects to be the value of the private attribute \code{best_model_list}
#' }
#' }
#'
#'
#' @importFrom R6 R6Class
#' @name CVSSForecaster
#' @export
#'
NULL
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
                      MAE = measures["MAE"], RMSE = measures["RMSE"],
                      MAPE = measures["MAPE"], MASE = measures["MASE"])
               }
               ))
             }
             ))
    },
    getBestModelList = function(){
      private$best_model_list
    },
    getBestAssessments = function(){
      assessments <- self$getAllAssessments()
      data.table::rbindlist(lapply(self$getCWENames(), function(cwe_name){
        selection <- as.data.frame(assessments[assessments$cwe == cwe_name, ])
        metrics <- t(tibble::column_to_rownames(
          as.data.frame(selection[,
                                  c("method", "MAE", "RMSE", "MAPE", "MASE")]),
          var = "method"))
        best <- nvdr::findBestColumn(metrics)
        list(cwe = cwe_name, method = best,
             MAE = selection[selection$method == best, ]$MAE,
             RMSE = selection[selection$method == best, ]$RMSE,
             MAPE = selection[selection$method == best, ]$MAPE,
             MASE = selection[selection$method == best, ]$MASE)
      }
      ))
    },
    useBest = function(fcast_period, arima_stepwise = F, arima_approx = F,
                       nnar_pi_sim = T){
      best_assessments <- self$getBestAssessments()[, c("cwe", "method")]
      apply(best_assessments, 1, function(cwe_and_method){
        cwe_name <- cwe_and_method[[1]]
        method_name <- cwe_and_method[[2]]
        compare <- nvdr::NameComparer$new(method_name)
        result <- NULL
        if (compare$isName("naive") | compare$isName("mean") |
            (compare$isName("random") & compare$isName("walk") &
             compare$isName("drift"))) {
          result <- self$getBenchmark(cwe_name)$useModel(fcast_period)
        } else if (compare$isName("linear") & compare$isName("regression")) {
          result <- self$getTSLinear(cwe_name)$useModel(fcast_period)
        } else if (compare$isName("ets")) {
          result <- self$getETS(cwe_name)$useModel(fcast_period)
        } else if (compare$isName("arima")) {
          result <- self$getARIMA(cwe_name)$useModel(
            fcast_period, stepwise = arima_stepwise,
            approximation = arima_approx)
        } else if (compare$isName("arfima")) {
          result <- self$getARFIMA(cwe_name)$useModel(fcast_period)
        } else if (compare$isName("baggedModel")) {
          result <- self$getBaggedETS(cwe_name)$useModel(fcast_period)
        } else if (compare$isName("nnar")) {
          result <- self$getNNAR(cwe_name)$useModel(fcast_period,
                                                    pi_simulation = nnar_pi_sim)
        } else if (compare$isName("basic") & compare$isName("structural")) {
          result <- self$getStructTS(cwe_name)$useModel(fcast_period)
        } else if (compare$isName("bats") | compare$isName("tbats")) {
          result <- self$getTBATS(cwe_name)$useModel(fcast_period)
        }
        rm(compare)
        list(cwe = cwe_name, future = result)
      }
      )
    },
    plotUseBest = function(bestInUse, row_no, col_no, compact = F, actual){
        if (missing(actual)) {
          actual_line <- function(actual, cwe) NULL
        } else {
          actual_line <- function(actual, cwe) forecast::autolayer(
            actual[, cwe], series = "Actual")
        }
         plots <- lapply(c(1:length(bestInUse)), function(list_index){
           cwe_name <- bestInUse[[list_index]]$cwe
           ylabtitle <- ggplot2::ylab(cwe_name)
           values <- bestInUse[[list_index]]$future
           aline <- actual_line(actual, cwe_name)
           if (compact) {
             ggplot2::autoplot(values) + ylabtitle +
               ggplot2::guides(fill = F) + ggplot2::ggtitle("") + aline
             } else {
               ggplot2::autoplot(values) + ylabtitle  + aline
             }
           }
           )
         gridExtra::marrangeGrob(plots, nrow = row_no, ncol = col_no)
    },
    assessUseBest = function(bestInUse, actual){
      data.table::rbindlist(lapply(c(1:length(bestInUse)), function(list_index){
        cwe_name <- bestInUse[[list_index]]$cwe
        ylabtitle <- ggplot2::ylab(cwe_name)
        values <- bestInUse[[list_index]]$future
        measure <- round(forecast::accuracy(
          values, actual[, cwe_name])["Test set", ], 4)
        list(cwe = cwe_name, method = bestInUse[[list_index]]$future$method,
             MAE = measure["MAE"], RMSE = measure["RMSE"],
             MAPE = measure["MAPE"], MASE = measure["MASE"])
      }
      ))
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
      best_assessments <- self$getBestAssessments()[, c("cwe", "method")]
      private$best_model_list <-
        apply(best_assessments, 1, function(cwe_and_method){
          cwe_name <- cwe_and_method[[1]]
          method_name <- cwe_and_method[[2]]
          compare <- nvdr::NameComparer$new(method_name)
          result <- NULL
          if (compare$isName("naive") | compare$isName("mean") |
              (compare$isName("random") &  compare$isName("walk")
               & compare$isName("drift"))
              ) {
            result <- self$getBenchmark(cwe_name)
          } else if (compare$isName("linear") & compare$isName("regression")) {
            result <- self$getTSLinear(cwe_name)
          } else if (compare$isName("ets")) {
            result <- self$getETS(cwe_name)
          } else if (compare$isName("arima")) {
            result <- self$getARIMA(cwe_name)
          } else if (compare$isName("arfima")) {
            result <- self$getARFIMA(cwe_name)
          } else if (compare$isName("baggedModel")) {
            result <- self$getBaggedETS(cwe_name)
          } else if (compare$isName("nnar")) {
            result <- self$getNNAR(cwe_name)
          } else if (compare$isName("basic") & compare$isName("structural")) {
            result <- self$getStructTS(cwe_name)
          } else if (compare$isName("bats") | compare$isName("tbats")) {
            result <- self$getTBATS(cwe_name)
          }
          rm(compare)
          result
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
