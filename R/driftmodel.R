DriftModel <- R6::R6Class(
  "DriftModel",
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
      train <- super$getTrainingSet()
      new_train <- ts(c(train, super$getTestSet()), start = start(train),
                      frequency = frequency(train))
      fcast_bc <- function() private$applyForecast(
        box_cox = T,
        bstrap = !super$isBootstrapNotUsed(),
        use_train = new_train,
        use_period = fcast_period)
      fcast <- function() private$applyForecast(
        bstrap = !super$isBootstrapNotUsed(),
        use_train = new_train,
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
        forecast::rwf(train, h = fcast_period,
                        lambda = forecast::BoxCox.lambda(train),
                        bootstrap = bstrap, drift = T)
      } else {
        forecast::rwf(train, h = fcast_period, bootstrap = bstrap, drift = T)
      }
    }
  )
)
