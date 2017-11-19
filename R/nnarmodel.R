NNARModel <- R6::R6Class(
  "NNARModel",
  inherit = FitModel,

  public = list(
    setFittedFcasted = function(fit_plain, fit_bc){
      test <- super$getTestSet()
      fcast_period <- super$getFcastPeriod()
      fcast_bc <- forecast::forecast(fit_bc, PI = pi_simulation,
                                     h = fcast_period)
      fcast <- forecast::forecast(fit_plain, PI = pi_simulation,
                                  h = fcast_period)
      if (super$rmseAccuracy(fcast) <= super$rmseAccuracy(fcast_bc)) {
        super$setFitted(fit_plain)
        super$setFcasted(fcast)
      } else {
        super$setFitted(fit_bc)
        super$setFcasted(fcast_bc)
        super$setBoxCoxApplied(T)
      }
    },
    buildModel = function(...){
      arguments <- list(...)
      pi_simulation <- arguments$pi_simulation
      train <- super$getTrainingSet()
      fit_bc <- forecast::nnetar(train, lambda = super$findLambda(train))
      fit <- forecast::nnetar(train)
      self$setFittedFcasted(fit, fit_bc)
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
