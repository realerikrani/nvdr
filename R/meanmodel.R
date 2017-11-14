MeanModel <- R6::R6Class(
  "MeanModel",
  inherit = NVDModel,

  public = list(
    buildModel = function(){
      train <- super$getTrainingSet()
      test <- super$getTestSet()
      fcast_period <- super$getFcastPeriod()
      fcast <- forecast::meanf(train, h = fcast_period)
      fcast_bc <- forecast::meanf(train, h = fcast_period,
                                  lambda = forecast::BoxCox.lambda(train))
      if (forecast::accuracy(fcast, test)[super$getTSetChar(), "RMSE"] <=
          forecast::accuracy(fcast_bc, test)[super$getTSetChar(), "RMSE"]) {
        super$setFcasted(fcast)
      } else {
        super$setFcasted(fcast_bc)
        super$setBoxCoxApplied(T)
      }
      residuals <- zoo::na.approx(super$getFcasted()$residuals)
      super$testResidualsRandomnessBox(residuals, 1)
      super$testResidualsNormality(residuals)

      if (super$areResidualsNotNormal() & !super$areResidualsNotRandom()) {
        if (super$isBoxCoxApplied()) {
          super$setFcasted(
            forecast::meanf(train, h = fcast_period,
                            lambda = forecast::BoxCox.lambda(train),
                            bootstrap = T))
        } else {
          super$setFcasted(forecast::meanf(train, h = fcast_period,
                                           bootstrap = T))
        }
        super$setBootstrapNotUsed(F)
      }
    }
  )
)