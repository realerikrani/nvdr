BaggedETSModel <- R6::R6Class(
  "BaggedETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      private$fit <- forecast::baggedETS(super$getTrainingSet())
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
      super$setPiIgnored(TRUE)
      }
    )
  )
