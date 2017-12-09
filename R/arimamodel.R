ARIMAModel <- R6::R6Class(
  "ARIMAModel",
  inherit = FitModel,

  public = list(
    buildModel = function(...){
      super$setFittedFcasted(private$fitARIMA, private$testRandomness,
                             private$fcastARIMA, ...)
    },
    useModel = function(fcast_period, ...){
      super$executeUseModel(private$fitARIMA, private$fcastARIMA, fcast_period,
                            ...)
    }
  ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBox(residuals,
                                       length(super$getFitted()$coef)))
      },
    fitARIMA = function(train, ...){
      arguments <- list(...)
      sw <- arguments$stepwise
      appr <- arguments$approximation
      forecast::auto.arima(train, lambda = super$findLambda(train),
                           stepwise = ifelse(length(sw),sw,T),
                           approximation = ifelse(length(appr),appr,T))
    },
    fcastARIMA = function(fitted_model, fcast_period, ...){
      arguments <- list(...)
      btstrp <- arguments$bootstrap
      forecast::forecast(fitted_model, h = fcast_period,
                         bootstrap = ifelse(length(btstrp) > 1, btstrp, F))
    }
  )
)
