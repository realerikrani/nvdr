ARFIMAModel <- R6::R6Class(
  "ARFIMAModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit_bc <- forecast::arfima(train, lambda = forecast::BoxCox.lambda(train))
      fit <- forecast::arfima(train)
      if (forecast::accuracy(fit$fitted, x = train)[, "RMSE"] <=
          forecast::accuracy(fit_bc$fitted, x = train)[,"RMSE"]) {
        private$fit <- fit
      } else {
        private$fit <- fit_bc
      }
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
      super$setPiIgnored(TRUE)
    }
  )
)