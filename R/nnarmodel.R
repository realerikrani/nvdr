NNARModel <- R6::R6Class(
  "NNARModel",
  inherit = FitModel,

  public = list(
    buildModel = function(...){
      super$setFittedFcasted(private$fitNNAR, private$testRandomness,
                             private$fcastNNAR, ...)
    },
    useModel = function(fcast_period, ...){
      super$executeUseModel(private$fitNNAR, private$fcastNNAR, fcast_period,
                            ...)
    }
  ),
  private = list(
    testRandomness = function(residuals) NULL,
    fitNNAR = function(train, ...){
      arguments <- list(...)
      pi <- arguments$pi_simulation
      if (length(pi) > 0) {
        super$setPiIgnored(pi)
      } else {
        super$setPiIgnored(T)
      }
      forecast::nnetar(train, lambda = super$findLambda(train))
    },
    fcastNNAR = function(fitted_model, fcast_period, ...){
      arguments <- list(...)
      btstrp <- arguments$bootstrap
      pi <- arguments$pi_simulation
      forecast::forecast(fitted_model, h = fcast_period,
                         bootstrap = ifelse(length(btstrp) > 0, btstrp, F),
                         PI = ifelse(length(pi) > 0, pi, F))
    }
  )
)
