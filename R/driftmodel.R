DriftModel <- R6::R6Class(
  "DriftModel",
  inherit = NVDModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fcast <- forecast::rwf(train, h = private$fcast_period, drift = TRUE)
      fcast_bc <- forecast::rwf(train,
                                h = private$fcast_period, drift = TRUE,
                                lambda = forecast::BoxCox.lambda(train))
      if (forecast::accuracy(fcast)[,"RMSE"] <=
          forecast::accuracy(fcast_bc)[,"RMSE"]) {
        private$fcast <-  fcast
      } else {
        private$fcast <- fcast_bc
      }
      residuals <- zoo::na.approx(super$getFcasted()$residuals)
      super$testResidualsRandomnessBox(residuals, 1)
      super$testResidualsNormality(residuals)
    }
  )
)
