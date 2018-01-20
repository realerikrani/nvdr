#' Class ARIMAModel
#'
#' Represents ARIMA models.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/auto.arima.html}{forecast::auto.arima}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel(...)} passes on parameters \code{stepwise} and \code{approximation} and fitting and forecasting functions to \link{FitModel}'s method \code{setFittedFcasted}; applies Box-Cox transformation and bias-adjustment; passes function which checks whether residuals are not independently distributed; allows generating forecast intervals from bootstrapped residuals if necessary
#'   \item \strong{useModel(fcast_period, ...)} calls \link{FitModel}'s method \code{executeUseModel} to fit model and forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name ARIMAModel
#' @export
#'
NULL
ARIMAModel <- R6::R6Class(
  "ARIMAModel",
  inherit = FitModel,

  public = list(
    buildModel = function(...){
      super$setFittedFcasted(private$fitARIMA, private$testRandomness,
                             private$fcastARIMA, ...)
    },
    useModel = function(fcast_period, ...){
      super$executeUseModel(private$fitARIMA, private$fcastARIMA, fcast_period,
                            ...)
    }
  ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBox(residuals,
                                       length(super$getFitted()$coef)))
      },
    fitARIMA = function(train, ...){
      arguments <- list(...)
      sw <- arguments$stepwise
      appr <- arguments$approximation
      forecast::auto.arima(train, lambda = super$findLambda(train),
                           stepwise = ifelse(length(sw) > 0, sw, T),
                           approximation = ifelse(length(appr) > 0, appr, T),
                           biasadj = T)
    },
    fcastARIMA = function(fitted_model, fcast_period, ...){
      arguments <- list(...)
      btstrp <- arguments$bootstrap
      forecast::forecast(fitted_model, h = fcast_period,
                         bootstrap = ifelse(length(btstrp) > 0, btstrp, F))
    }
  )
)
