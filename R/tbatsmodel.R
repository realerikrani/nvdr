#' Class TBATSModel
#'
#' Represents TBATS and BATS models.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/tbats.html}{forecast::tbats}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel()} passes on fitting and forecasting functions to \link{FitModel}'s method \code{setFittedFcasted}; applies bias-adjustment; passes function which checks whether residuals are not independently distributed
#'   \item \strong{useModel(fcast_period)} calls \link{FitModel}'s method \code{executeUseModel} to fit model and forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name TBATSModel
#' @export
#'
NULL
TBATSModel <- R6::R6Class(
  "TBATSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitTBATS, private$testRandomness,
                             private$fcastTBATS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitTBATS, private$fcastTBATS, fcast_period)
    }
    ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBox(residuals,
        length(super$getFitted()$parameters$vect) +
          NROW(super$getFitted()$seed.states)))
    },
    fitTBATS = function(train){
      suppressWarnings(forecast::tbats(train, biasadj = T))
    },
    fcastTBATS = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
  )
