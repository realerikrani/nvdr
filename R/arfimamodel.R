ARFIMAModel <- R6::R6Class(
  "ARFIMAModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitARFIMA, private$testRandomness,
                             private$fcastARFIMA)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitARFIMA, private$fcastARFIMA,
                            fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitARFIMA = function(train){
      super$setPiIgnored(T)
      forecast::arfima(train, lambda = super$findLambda(train), biasadj = T)
    },
    fcastARFIMA = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
)
