SeasonalNaiveModel <- R6::R6Class(
  "SeasonalNaiveModel",
  inherit = NVDModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fcast <- forecast::snaive(train, h = private$fcast_period)
      fcast_bc <- forecast::snaive(train, h = private$fcast_period,
                                  lambda = forecast::BoxCox.lambda(train))
      if (forecast::accuracy(fcast)[,"RMSE"] <=
          forecast::accuracy(fcast_bc)[,"RMSE"]) {
        private$fcast <-  fcast
      } else {
        private$fcast <- fcast_bc
      }
      residuals <- zoo::na.approx(super$getFcasted()$residuals)
      super$testResidualsRandomnessBox(residuals, 0)
      super$testResidualsNormality(residuals)
    }
  )
)
