MeanModel <- R6::R6Class(
  "MeanModel",
  inherit = FcastModel,

  public = list(
    buildModel = function(){
      super$considerBoxCox(private$applyForecast(),
                           private$applyForecast(box_cox = T))
      super$analyseResiduals(1)
      super$considerBootstrap(function() private$applyForecast(bstrap = T),
                              function() private$applyForecast(box_cox = T,
                                                               bstrap = T))
    },
    useModel = function(fcast_period){
      fcast_bc <- function(train) private$applyForecast(
        box_cox = T,
        bstrap = !super$isBootstrapNotUsed(),
        use_train = train,
        use_period = fcast_period)
      fcast <- function(train) private$applyForecast(
        bstrap = !super$isBootstrapNotUsed(),
        use_train = train,
        use_period = fcast_period)
      super$executeUseModel(fcast, fcast_bc)
    }
  ),
  private = list(
    applyForecast = function(box_cox = F, bstrap = F, use_train, use_period){
      if (missing(use_train) | missing(use_period)) {
        train <- super$getTrainingSet()
        fcast_period <- super$getFcastPeriod()
      } else {
        train <- use_train
        fcast_period <- use_period
      }
      if (box_cox) {
        forecast::meanf(train, h = fcast_period,
                         lambda = super$findLambda(train),
                         bootstrap = bstrap)
      } else {
        forecast::meanf(train, h = fcast_period, bootstrap = bstrap)
      }
    }
  )
)
