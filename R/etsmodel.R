ETSModel <- R6::R6Class(
  "ETSModel",
  inherit = FitModel,

  public = list(
    buildModel = function(){
      super$setFittedFcasted(private$fitETS, private$testRandomness,
                             private$fcastETS)
    },
    useModel = function(fcast_period){
      super$executeUseModel(private$fitETS, private$fcastETS, fcast_period)
    }
  ),
  private = list(
    testRandomness = function(residuals) {
      super$setResidualsNotRandom(super$testResidualsRandomnessBox(residuals,
                                       length(super$getFitted()$par)))
      },
    fitETS = function(train){
      forecast::ets(train, lambda = super$findLambda(train), biasadj = T)
    },
    fcastETS = function(fitted_model, fcast_period, ...){
      arguments <- list(...)
      btstrp <- arguments$bootstrap
      forecast::forecast(fitted_model, h = fcast_period,
                         bootstrap = ifelse(length(btstrp) > 1, btstrp, F))
    }
  )
  )
