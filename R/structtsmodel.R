StructTSModel <- R6::R6Class(
  "StructTSModel",
  inherit = FitModel,

  public = list(
    setFittedFcasted = function(){
      super$setFitted(stats::StructTS(super$getTrainingSet()))
      super$setFcasted(forecast::forecast(super$getFitted(),
                                          h = private$fcast_period))
    },
    buildModel = function(){
      self$setFittedFcasted()
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
      super$setPiIgnored(TRUE)
    }
  )
)
