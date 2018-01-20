#' Class StructTSModel
#'
#' Represents basic structural models (BSM).
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \code{stats::StructTS}
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
#' @name StructTSModel
#' @export
#'
NULL
StructTSModel <- R6::R6Class(
  "StructTSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitStructTS, private$testRandomness,
                             private$fcastStructTS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitStructTS, private$fcastStructTS,
                            fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitStructTS = function(train){
      super$setPiIgnored(T)
      stats::StructTS(train, type = "BSM")
    },
    fcastStructTS = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
)
