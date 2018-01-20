#' Class NNARModel
#'
#' Represents Feed-forward single-hidden-layer neural network autoregression models.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FitModel}
#' }
#' }
#'
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/nnetar.html}{forecast::nnetar}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel(...)} passes on parameter \code{PI} and fitting and forecasting functions to \link{FitModel}'s method \code{setFittedFcasted}; applies Box-Cox transformation without bias-adjustment; ignores passing function which checks whether residuals are not independently distributed
#'   \item \strong{useModel(fcast_period, ...)} calls \link{FitModel}'s method \code{executeUseModel} to fit model and forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @importFrom R6 R6Class
#' @name NNARModel
#' @export
#'
NULL
NNARModel <- R6::R6Class(
  "NNARModel",
  inherit = FitModel,

  public = list(
    buildModel = function(...){
      super$setFittedFcasted(private$fitNNAR, private$testRandomness,
                             private$fcastNNAR, ...)
    },
    useModel = function(fcast_period, ...){
      super$executeUseModel(private$fitNNAR, private$fcastNNAR, fcast_period,
                            ...)
    }
  ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitNNAR = function(train, ...){
      super$setPiIgnored(T)
      forecast::nnetar(train, lambda = super$findLambda(train))
    },
    fcastNNAR = function(fitted_model, fcast_period, ...){
      arguments <- list(...)
      btstrp <- arguments$bootstrap
      pi <- arguments$pi_simulation
      forecast::forecast(fitted_model, h = fcast_period,
                         bootstrap = ifelse(length(btstrp) > 0, btstrp, F),
                         PI = ifelse(length(pi) > 0, pi, F))
    }
  )
)
