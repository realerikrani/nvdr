NNARModel <- R6::R6Class(
  "NNARModel",
  inherit = FitModel,

  public = list(
    setFittedFcasted = function(fit_plain, fit_bc){
      fcast_bc <- forecast::forecast(fit_bc, PI = pi_simulation,
                                  h = private$fcast_period)
      fcast <- forecast::forecast(fit_plain, PI = pi_simulation,
                                          h = private$fcast_period)
      if (forecast::accuracy(fcast)[, "RMSE"] <=
          forecast::accuracy(fcast_bc)[, "RMSE"]) {
        self$setFitted(fit_plain)
        super$setFcasted(fcast)
      } else {
        self$setFitted(fit_bc)
        super$setFcasted(fcast_bc)
      }
    },
    buildModel = function(...){
      arguments <- list(...)
      pi_simulation <- arguments$pi_simulation
      train <- super$getTrainingSet()
      fit_bc <- forecast::nnetar(train,lambda = forecast::BoxCox.lambda(train))
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