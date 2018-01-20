#' Class FcastModel
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{NVDModel}
#' }
#' }
#'
#' @section Direct Subclasses:{
#' \itemize{
#' \item \link{BenchmarkModel},
#' \item \link{MeanModel},
#' \item \link{DriftModel},
#' \item \link{NaiveModel},
#' \item \link{SeasonalNaiveModel}
#' }
#' }
#
#'
#' @section Private attributes:{
#' \itemize{
#'   \item \strong{box_cox_applied} logical value indicating whether the Box-Cox transformation was applied (default F)
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{considerBoxCox(fcast, fcast_bc)} selects the best forecasts between those where the Box-Cox transformation is used and those, where it is not used; sets thre result by using \code{super$setFcasted}
#'   \item \strong{analyseResiduals(df)} analyses the residuals with the given degree of freedom value
#'   \item \strong{considerBootstrap(fcast, fcast_bc)} generates forecast intervals from bootstrapped residuals when \code{super$areResidualsNotNormal() & !super$areResidualsNotRandom()}; takes into account whether the Box-Cox transformation was beneficial to use or not
#'   \item \strong{executeUseModel(fcast, fcast_bc)} calls the forecast function with training set obtained by calling \code{super$getMergedTrainTestSet()}; applies the Box-Cox transformation when \code{self$isBoxCoxApplied() == TRUE}
#'   \item \strong{isBoxCoxApplied()} returns private attribute \code{box_cox_applied}
#'   \item \strong{setBoxCoxApplied(logical_value)} assings the \code{logical_value} to private attribute \code{box_cox_applied}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name FcastModel
#' @export
#'
NULL
FcastModel <- R6::R6Class(
  "FcastModel",
  inherit = NVDModel,

  public = list(
    considerBoxCox = function(fcast, fcast_bc){
      if (super$compareCombinedAccuracy(fcast, fcast_bc)) {
        super$setFcasted(fcast)
      } else {
        super$setFcasted(fcast_bc)
        self$setBoxCoxApplied(T)
      }
    },
    analyseResiduals = function(df){
      residuals <- super$getResiduals(super$getFcasted())
      super$setResidualsNotRandom(
        super$testResidualsRandomnessBox(residuals, df))
      super$setResidualsNotNormal(super$testResidualsNormality(residuals))
    },
    considerBootstrap = function(fcast, fcast_bc){
      if (super$areResidualsNotNormal() & !super$areResidualsNotRandom()) {
        if (self$isBoxCoxApplied()) {
          super$setFcasted(do.call(fcast_bc, list()))
        } else {
          super$setFcasted(do.call(fcast, list()))
        }
        super$setBootstrapNotUsed(F)
      }
    },
    executeUseModel = function(fcast, fcast_bc){
      new_train <- super$getMergedTrainTestSet()
      if (self$isBoxCoxApplied()) {
        fc <- do.call(fcast_bc, list(new_train))
      } else {
        fc <- do.call(fcast, list(new_train))
      }
      return(fc)
    },
    isBoxCoxApplied = function(){
      private$box_cox_applied
    },
    setBoxCoxApplied = function(logical_value){
      private$box_cox_applied <- logical_value
    }
  ),
  private = list(
    box_cox_applied = F
  )
)
