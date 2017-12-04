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
      super$setPiIgnored(T)
    },
    useModel = function(fcast_period, residuals_check = T){
      fit_bc <- function(new_train) forecast::arfima(
        new_train, lambda = super$findLambda(new_train))
      fit <- function(new_train) forecast::arfima(new_train)
      fcast <- function(ft) forecast::forecast(ft, h = fcast_period)
      super$executeUseModel(fit, fit_bc, fcast, residuals_check)
    }
  )
)
