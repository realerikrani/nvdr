ETSModel <- R6::R6Class(
  "ETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit_bc <- forecast::ets(train,lambda = forecast::BoxCox.lambda(train))
      fit <- forecast::ets(train)
      if (fit$aicc <= fit_bc$aicc) {
        private$fit <- fit
      } else {
        private$fit <- fit_bc
      }
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsRandomnessBox(residuals, length(super$getFitted()$par))
      super$testResidualsNormality(residuals)
    }
  )
)
