NNARModel <- R6::R6Class(
  "NNARModel",
  inherit = FitModel,

  public = list(
    buildModel = function(...){
      arguments <- list(...)
      pi_simulation <- arguments$pi_simulation
      train <- super$getTrainingSet()
      private$fit <- forecast::nnetar(super$getTrainingSet())
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)
      fit_bc <- forecast::nnetar(train,lambda = forecast::BoxCox.lambda(train))
      fit <- forecast::nnetar(train)
      if (forecast::accuracy(fit)[, "RMSE"] <=
          forecast::accuracy(fit_bc)[, "RMSE"]) {
        private$fit <-  fit
      } else {
        private$fit <- fit_bc
      }
      private$fcast <- forecast::forecast(super$getFitted(), PI = pi_simulation,
                                          h = private$fcast_period)
      if (pi_simulation) {
        super$setPiIgnored(FALSE)
      } else {
        super$setPiIgnored(TRUE)
      }
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
    }
  )
)