FitModel <- R6::R6Class(
  "FitModel",
  inherit = NVDModel,

  public = list(
    getFitted = function(){
      private$fit
    },
    setFitted = function(fit_result){
      private$fit <- fit_result
    },
    setFittedFcasted = function(fit_plain, fit_bc){
      fcast_bc <- forecast::forecast(fit_bc, h = private$fcast_period)
      fcast <- forecast::forecast(fit_plain, h = private$fcast_period)
      if (forecast::accuracy(fcast)[, "RMSE"] <=
          forecast::accuracy(fcast_bc)[, "RMSE"]) {
        self$setFitted(fit_plain)
        super$setFcasted(fcast)
      } else {
        self$setFitted(fit_bc)
        super$setFcasted(fcast_bc)
      }
    }
  ),

  private = list(
    fit = NA
  )
)
