TSLinearModel <- R6::R6Class(
  "TSLinearModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit <- forecast::tslm(train ~ trend + season)
      fit_bc <- forecast::tslm(train ~ trend + season,
                               lambda = super$findLambda(train))
      super$setFittedFcasted(fit, fit_bc)
      residuals <- super$getFitted()$residuals
      super$testResidualsRandomnessBreusch(
        residuals,
        length(super$getFitted()$model$coefficients),
        super$getFitted())
      super$testResidualsNormality(residuals)
    },
    useModel = function(fcast_period, residuals_check = T){
      fit_bc <- function(new_train) forecast::tslm(
        new_train ~ trend + season,
        lambda = super$findLambda(new_train))
      fit <- function(new_train) forecast::tslm(new_train ~ trend + season)
      fcast <- function(ft) forecast::forecast(
        ft, h = fcast_period, bootstrap = !super$isBootstrapNotUsed())
      super$executeUseModel(fit, fit_bc, fcast, residuals_check)
    }
  )
)
