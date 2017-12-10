TBATSModel <- R6::R6Class(
  "TBATSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitTBATS, private$testRandomness,
                             private$fcastTBATS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitTBATS, private$fcastTBATS, fcast_period)
    }
    ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBox(residuals,
        length(super$getFitted()$parameters$vect) +
          NROW(super$getFitted()$seed.states)))
    },
    fitTBATS = function(train){
      suppressWarnings(forecast::tbats(train, biasadj = T))
    },
    fcastTBATS = function(fitted_model, fcast_period, ...){
      forecast::forecast(fitted_model, h = fcast_period)
    }
  )
  )
