BaggedETSModel <- R6::R6Class(
  "BaggedETSModel",
  inherit = FitModel,

  public = list(
    setFittedFcasted = function(){
      super$setFitted(forecast::baggedModel(super$getTrainingSet(),
                                            fn = "ets"))
      super$setFcasted(forecast::forecast(super$getFitted(),
                                          h = super$getFcastPeriod()))
    },
    buildModel = function(){
      self$setFittedFcasted()
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
      super$setPiIgnored(T)
    },
    useModel = function(fcast_period, residuals_check = T){
      fit <- function(new_train) forecast::baggedModel(new_train, fn = "ets")
      fcast <- function(ft) forecast::forecast(ft, h = fcast_period)
      super$executeUseModel(fit, NULL, fcast, residuals_check)
    }
    )
  )
