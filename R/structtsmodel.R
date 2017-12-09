StructTSModel <- R6::R6Class(
  "StructTSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitStructTS, private$testRandomness,
                             private$fcastStructTS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitStructTS, private$fcastStructTS,
                            fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitStructTS = function(train){
      super$setPiIgnored(T)
      stats::StructTS(train, type = "BSM")
    },
    fcastStructTS = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
)
