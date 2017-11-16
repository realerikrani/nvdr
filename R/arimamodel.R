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
        forecast::auto.arima(train, lambda = forecast::BoxCox.lambda(train),
                             stepwise = stepw, approximation = approx)
      fit <- forecast::auto.arima(train, stepwise = stepw,
                                  approximation = approx)
      super$setFittedFcasted(fit, fit_bc)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsRandomnessBox(residuals,
                                       length(super$getFitted()$coef))
      super$testResidualsNormality(residuals)
      super$considerBootstrap(fit, fit_bc)
    }
  )
)
