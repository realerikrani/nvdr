StructTSModel <- R6::R6Class(
  "StructTSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      private$fit <- stats::StructTS(super$getTrainingSet())
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
      super$setPiIgnored(TRUE)
    }
  )
)
