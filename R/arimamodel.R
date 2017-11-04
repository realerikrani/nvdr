ARIMAModel <- R6::R6Class(
  "ARIMAModel",
  inherit = FitModel,

  public = list(
    buildModel = function(...){
      arguments <- list(...)
      stepw <- arguments$stepwise
      approx <- arguments$approximation
      train <- super$getTrainingSet()
      fit_bc <-
        forecast::auto.arima(train,lambda = forecast::BoxCox.lambda(train),
                             stepwise = stepw, approximation = approx)
      fit <- forecast::auto.arima(train, stepwise = stepw,
                                  approximation = approx)
      if (fit$aicc <= fit_bc$aicc) {
        private$fit <- fit
      } else {
        private$fit <- fit_bc
      }
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsRandomnessBox(residuals,
                                       length(super$getFitted()$coef))
      super$testResidualsNormality(residuals)
    }
  )
)