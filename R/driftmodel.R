DriftModel <- R6::R6Class(
  "DriftModel",
  inherit = NVDModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      test <- super$getTestSet()
      fcast_period <- super$getFcastPeriod()
      fcast <- forecast::rwf(train, h = fcast_period, drift = TRUE)
      fcast_bc <- forecast::rwf(train, h = fcast_period, drift = TRUE,
                                lambda = forecast::BoxCox.lambda(train))
      if (forecast::accuracy(fcast, test)[super$getTSetChar(), "RMSE"] <=
          forecast::accuracy(fcast_bc, test)[super$getTSetChar(), "RMSE"]) {
        super$setFcasted(fcast)
      } else {
        super$setFcasted(fcast_bc)
      }
      residuals <- zoo::na.approx(super$getFcasted()$residuals)
      super$testResidualsRandomnessBox(residuals, 1)
      super$testResidualsNormality(residuals)
    }
  )
)
