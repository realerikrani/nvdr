ETSModel <- R6::R6Class(
  "ETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      fit_bc <- forecast::ets(train, lambda = super$findLambda(train))
      fit <- forecast::ets(train)
      super$setFittedFcasted(fit, fit_bc)
      residuals <- zoo::na.approx(super$getFitted()$residuals)
      super$testResidualsRandomnessBox(residuals, length(super$getFitted()$par))
      super$testResidualsNormality(residuals)
      super$considerBootstrap(fit, fit_bc)
    },
    useModel = function(fcast_period, residuals_check = T){
      fit_bc <- function(new_train) forecast::ets(
        new_train, lambda = super$findLambda(new_train))
      fit <- function(new_train) forecast::ets(new_train)
      fcast <- function(ft) forecast::forecast(ft,
                                  h = fcast_period,
                                  bootstrap = !super$isBootstrapNotUsed())
      super$executeUseModel(fit, fit_bc, fcast, residuals_check)
    }
    )
  )
