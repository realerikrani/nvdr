#' Class TSLinearModel
#'
#' Represents time series linear regression models with "trend" and "season" variables.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/tslm.html}{forecast::tslm}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel()} passes on fitting and forecasting functions to \link{FitModel}'s method \code{setFittedFcasted}; applies Box-Cox transformation and bias-adjustment; passes function which checks whether residuals are not independently distributed
#'   \item \strong{useModel(fcast_period)} calls \link{FitModel}'s method \code{executeUseModel} to fit model and forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name TSLinearModel
#' @export
#'
NULL
TSLinearModel <- R6::R6Class(
  "TSLinearModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitTSLinear, private$testRandomness,
                             private$fcastTSLinear)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitTSLinear, private$fcastTSLinear,
                            fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBreusch(
              residuals,
              length(super$getFitted()$model$coefficients), super$getFitted()))
      },
    fitTSLinear = function(train){
      forecast::tslm(train ~ trend + season, lambda = super$findLambda(train),
                     biasadj = T)
    },
    fcastTSLinear = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
)
