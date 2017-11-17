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
    },
    useModel = function(fcast_period){
      fit_bc <- function(new_train) forecast::auto.arima(
        new_train, lambda = forecast::BoxCox.lambda(new_train),
        stepwise = F, approximation = F)
      fit <- function(new_train) forecast::auto.arima(new_train,
                                                      stepwise = F,
                                                      approximation = F)
      fcast <- function(ft) forecast::forecast(
        ft, h = fcast_period, bootstrap = !super$isBootstrapNotUsed())
      super$executeUseModel(fit, fit_bc, fcast)
    }
  )
)
