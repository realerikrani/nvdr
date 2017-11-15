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
      test <- super$getTestSet()
      fcast_period <- super$getFcastPeriod()
      fcast_bc <- forecast::forecast(fit_bc, h = fcast_period)
      fcast <- forecast::forecast(fit_plain, h = fcast_period)
      if (forecast::accuracy(fcast, test)[super$getTSetChar(), "RMSE"] <=
          forecast::accuracy(fcast_bc, test)[super$getTSetChar(), "RMSE"]) {
        self$setFitted(fit_plain)
        super$setFcasted(fcast)
      } else {
        self$setFitted(fit_bc)
        super$setFcasted(fcast_bc)
      }
    },
    considerBootstrap = function(fit_plain, fit_bc){
      if (super$areResidualsNotNormal() & !super$areResidualsNotRandom()) {
        fcast_period <- super$getFcastPeriod()
        if (super$isBoxCoxApplied()) {
          super$setFcasted(forecast::forecast(fit_bc, h = fcast_period,
                                              bootstrap = T))
        } else {
          super$setFcasted(forecast::forecast(fit_plain, h = fcast_period,
                                              bootstrap = T))
        }
        super$setBootstrapNotUsed(F)
      }
    }
  ),

  private = list(
    fit = NA
  )
)
