TSLinearModel <- R6::R6Class(
  "TSLinearModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitTSLinear, private$testRandomness,
                             private$fcastTSLinear)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitTSLinear, private$fcastTSLinear,
                            fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBreusch(
              residuals,
              length(super$getFitted()$model$coefficients), super$getFitted()))
      },
    fitTSLinear = function(train){
      forecast::tslm(train ~ trend + season, lambda = super$findLambda(train),
                     biasadj = T)
    },
    fcastTSLinear = function(fitted_model, fcast_period, ...){
      arguments <- list(...)
      btstrp <- arguments$bootstrap
      forecast::forecast(fitted_model,
                         h = fcast_period,
                         bootstrap = ifelse(length(btstrp) > 1, btstrp, F))
    }
  )
)
