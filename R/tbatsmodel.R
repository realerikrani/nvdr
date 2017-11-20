TBATSModel <- R6::R6Class(
  "TBATSModel",
  inherit = FitModel,

  public = list(
    setFittedFcasted = function(){
      # No need to output text about missing values or optimization convergence
      super$setFitted(suppressWarnings(forecast::tbats(super$getTrainingSet())))
      super$setFcasted(forecast::forecast(super$getFitted(),
                                          h = super$geFcastPeriod()))
    },
    buildModel = function(){
      self$setFittedFcasted()
      residuals <- zoo::na.approx(stats::residuals(super$getFitted()))
      super$testResidualsRandomnessBox(
        residuals,
        length(super$getFitted()$parameters$vect) +
          NROW(super$getFitted()$seed.states))
      super$testResidualsNormality(residuals)
    },
    useModel = function(fcast_period, residuals_check = T){
      warning("Not implemented!")
    }
    )
  )
