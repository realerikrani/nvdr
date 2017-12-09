BaggedETSModel <- R6::R6Class(
  "BaggedETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitBaggedETS, private$testRandomness,
                             private$fcastBaggedETS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitBaggedETS, private$fcastBaggedETS,
                            fcast_period)
    }
    ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitBaggedETS = function(train){
      super$setPiIgnored(T)
      forecast::baggedETS(train)
    },
    fcastBaggedETS = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
  )
