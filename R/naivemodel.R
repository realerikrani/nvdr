NaiveModel <- R6::R6Class(
  "NaiveModel",
  inherit = FcastModel,

  public = list(
    buildModel = function(){
      super$considerBoxCox(private$applyForecast(),
                           private$applyForecast(box_cox = T))
      super$analyseResiduals(0)
      super$considerBootstrap(function() private$applyForecast(bstrap = T),
                              function() private$applyForecast(box_cox = T,
                                                               bstrap = T))
    }
  ),
  private = list(
    applyForecast = function(box_cox = F, bstrap = F){
      train <- super$getTrainingSet()
      fcast_period <- super$getFcastPeriod()
      if (box_cox) {
        forecast::naive(train, h = fcast_period,
                         lambda = forecast::BoxCox.lambda(train),
                         bootstrap = bstrap)
      } else {
        forecast::naive(train, h = fcast_period, bootstrap = bstrap)
      }
    }
  )
)
