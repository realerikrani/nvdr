TSLinearModel <- R6::R6Class(
  "TSLinearModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit <- forecast::tslm(train ~ trend + season)
      fit_bc <- forecast::tslm(train ~ trend + season,
                               lambda = forecast::BoxCox.lambda(train))
      super$setFittedFcasted(fit, fit_bc)
      residuals <- super$getFitted()$residuals
      super$testResidualsRandomnessBreusch(
        residuals,
        length(super$getFitted()$model$coefficients),
        super$getFitted())
      super$testResidualsNormality(residuals)
    }
  )
)
