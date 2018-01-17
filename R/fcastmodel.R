#' ...
#'
#' ...
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name FcastModel
#' @export
#'
NULL

#' @export
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
