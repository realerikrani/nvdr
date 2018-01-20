#' Class NaiveModel
#'
#' Represents naive models.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{FcastModel}
#' }
#' }
#
#' @section Important Dependencies: {
#' \itemize{
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/naive.html}{forecast::naive}
#' \item \href{http://pkg.robjhyndman.com/forecast/reference/forecast.html}{forecast::forecast}
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{buildModel()} passes on forecasting functions to \link{FcastModel}'s methods \code{considerBoxCox} and \code{considerBootstrap}; allows to choose whether forecast intervals should be generated from bootstrapped residuals and whether using Box-Cox transformation and bias-adjustment would result in better overall forecast accuracy; calls a \link{FcastModel}'s method \code{analyseResiduals} which checks whether residuals are not independently distributed and not normally distributed
#'   \item \strong{useModel(fcast_period)} calls \link{FcastModel}'s method \code{executeUseModel} to forecast \code{fcast_period}-step ahead future; passes on the same fitting and forecasting functions as \code{buildmodel()}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name NaiveModel
#' @export
#'
NULL
NaiveModel <- R6::R6Class(
  "NaiveModel",
  inherit = FcastModel,

  public = list(
    buildModel = function(){
      super$considerBoxCox(private$applyForecast(),
                           private$applyForecast(box_cox = T))
      super$analyseResiduals(0)
      super$considerBootstrap(function() private$applyForecast(bstrap = T),
                              function() private$applyForecast(box_cox = T,
                                                               bstrap = T))
    },
    useModel = function(fcast_period){
      fcast_bc <- function(train) private$applyForecast(
        box_cox = T,
        bstrap = !super$isBootstrapNotUsed(),
        use_train = train,
        use_period = fcast_period)
      fcast <- function(train) private$applyForecast(
        bstrap = !super$isBootstrapNotUsed(),
        use_train = train,
        use_period = fcast_period)
      super$executeUseModel(fcast, fcast_bc)
    }
  ),
  private = list(
    applyForecast = function(box_cox = F, bstrap = F, use_train, use_period){
      if (missing(use_train) | missing(use_period)) {
        train <- super$getTrainingSet()
        fcast_period <- super$getFcastPeriod()
      } else {
        train <- use_train
        fcast_period <- use_period
      }
      if (box_cox) {
        forecast::naive(train, h = fcast_period,
                         lambda = super$findLambda(train),
                         bootstrap = bstrap, biasadj = T)
      } else {
        forecast::naive(train, h = fcast_period, bootstrap = bstrap)
      }
    }
  )
)
