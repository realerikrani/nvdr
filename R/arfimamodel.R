#' Class ARFIMAModel
#'
#' Represents ARFIMA models.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/arfima.html}{forecast::arfima}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel()} passes on fitting and forecasting functions to \link{FitModel}'s method \code{setFittedFcasted}; applies Box-Cox transformation and bias-adjustment; ignores passing function which checks whether residuals are not independently distributed
#'   \item \strong{useModel(fcast_period)} calls \link{FitModel}'s method \code{executeUseModel} to fit model and forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name ARFIMAModel
#' @export
#'
NULL
ARFIMAModel <- R6::R6Class(
  "ARFIMAModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitARFIMA, private$testRandomness,
                             private$fcastARFIMA)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitARFIMA, private$fcastARFIMA,
                            fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitARFIMA = function(train){
      super$setPiIgnored(T)
      forecast::arfima(train, lambda = super$findLambda(train), biasadj = T)
    },
    fcastARFIMA = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
)
