TBATSModel <- R6::R6Class(
  "TBATSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      # No need to output text about missing values or optimization convergence
      private$fit <- suppressWarnings(forecast::tbats(super$getTrainingSet()))
      private$fcast <- forecast::forecast(super$getFitted(),
                                          h = private$fcast_period)

      residuals <- zoo::na.approx(stats::residuals(super$getFitted()))
      super$testResidualsRandomnessBox(
        residuals,
        length(super$getFitted()$parameters$vect) +
          NROW(super$getFitted()$seed.states))
      super$testResidualsNormality(residuals)
      }
    )
  )
