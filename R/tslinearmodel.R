TSLinearModel <- R6::R6Class(
  "TSLinearModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit <- forecast::tslm(train ~ trend + season)
      fit_bc <- forecast::tslm(train ~ trend + season,
                               lambda = forecast::BoxCox.lambda(train))
      if (forecast::accuracy(fit)[, "RMSE"] <=
          forecast::accuracy(fit_bc)[,"RMSE"]) {
        private$fit <-  fit
      } else {
        private$fit <- fit_bc
      }
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)
      residuals <- super$getFitted()$residuals
      super$testResidualsRandomnessBreusch(
        residuals,
        length(super$getFitted()$model$coefficients),
        super$getFitted())
      super$testResidualsNormality(residuals)
    }
  )
)
