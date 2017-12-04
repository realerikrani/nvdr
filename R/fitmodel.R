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
      fcast_period <- super$getFcastPeriod()
      fcast_bc <- forecast::forecast(fit_bc, h = fcast_period)
      fcast <- forecast::forecast(fit_plain, h = fcast_period)
      if (super$compareCombinedAccuracy(fcast, fcast_bc)) {
        self$setFitted(fit_plain)
        super$setFcasted(fcast)
      } else {
        self$setFitted(fit_bc)
        super$setFcasted(fcast_bc)
        super$setBoxCoxApplied(T)
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
    },
    executeUseModel = function(fit, fit_bc, fcast, residuals_check) {
      train <- super$getTrainingSet()
      new_train <- ts(c(train, super$getTestSet()), start = start(train),
                      frequency = frequency(train))
      if (super$isBoxCoxApplied()) {
        ft <- do.call(fit_bc, list(new_train))
      } else {
        ft <- do.call(fit, list(new_train))
      }
      fc <- do.call(fcast, list(ft))
      if (residuals_check) {
        forecast::checkresiduals(fc, plot = T)
      }
      return(fc)
    }
  ),

  private = list(
    fit = NA
  )
)
