ETSModel <- R6::R6Class(
  "ETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit_bc <- forecast::ets(train,lambda = forecast::BoxCox.lambda(train))
      fit <- forecast::ets(train)
      super$setFittedFcasted(fit, fit_bc)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsRandomnessBox(residuals, length(super$getFitted()$par))
      super$testResidualsNormality(residuals)
    }
  )
)
