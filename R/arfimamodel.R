ARFIMAModel <- R6::R6Class(
  "ARFIMAModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit_bc <- forecast::arfima(train, lambda = super$findLambda(train))
      fit <- forecast::arfima(train)
      super$setFittedFcasted(fit, fit_bc)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsNormality(residuals)
      super$setPiIgnored(TRUE)
    }
  )
)
