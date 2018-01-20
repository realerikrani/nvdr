#' Class ETSModel
#'
#' Represents ETS models.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/ets.html}{forecast::ets}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel()} passes on fitting and forecasting functions to \link{FitModel}'s method \code{setFittedFcasted}; applies Box-Cox transformation and bias-adjustment; passes function which checks whether residuals are not independently distributed; allows generating forecast intervals from bootstrapped residuals if necessary
#'   \item \strong{useModel(fcast_period)} calls \link{FitModel}'s method \code{executeUseModel} to fit model and forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name ETSModel
#' @export
#'
NULL
ETSModel <- R6::R6Class(
  "ETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitETS, private$testRandomness,
                             private$fcastETS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitETS, private$fcastETS, fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBox(residuals,
                                       length(super$getFitted()$par)))
      },
    fitETS = function(train){
      forecast::ets(train, lambda = super$findLambda(train), biasadj = T)
    },
    fcastETS = function(fitted_model, fcast_period, ...){
      arguments <- list(...)
      btstrp <- arguments$bootstrap
      forecast::forecast(fitted_model, h = fcast_period,
                         bootstrap = ifelse(length(btstrp) > 0, btstrp, F))
    }
  )
  )
