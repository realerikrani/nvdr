BaggedETSModel <- R6::R6Class(
  "BaggedETSModel",
  inherit = FitModel,

  public = list(
    setFittedFcasted = function(){
      super$setFitted(forecast::baggedETS(super$getTrainingSet()))
      super$setFcasted(forecast::forecast(super$getFitted(),
                                          h = super$getFcastPeriod()))
    },
    buildModel = function(){
      self$setFittedFcasted()
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
      super$setPiIgnored(TRUE)
    },
    useModel = function(fcast_period, residuals_check = T){
      warning("Not implemented!")
    }
    )
  )
