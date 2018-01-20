#' Class BaggedETSModel
#'
#' Represents Bagged ETS models.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/baggedModel.html}{forecast::baggedModel}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel()} passes on fitting and forecasting functions to \link{FitModel}'s method \code{setFittedFcasted}; ignores passing function which checks whether residuals are not independently distributed
#'   \item \strong{useModel(fcast_period)} calls \link{FitModel}'s method \code{executeUseModel} to fit model and forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name BaggedETSModel
#' @export
#'
NULL
BaggedETSModel <- R6::R6Class(
  "BaggedETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitBaggedETS, private$testRandomness,
                             private$fcastBaggedETS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitBaggedETS, private$fcastBaggedETS,
                            fcast_period)
    }
    ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitBaggedETS = function(train){
      super$setPiIgnored(T)
      forecast::baggedModel(train, fn = "ets")
    },
    fcastBaggedETS = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
  )
