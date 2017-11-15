FcastModel <- R6::R6Class(
  "FcastModel",
  inherit = NVDModel,

  public = list(
    considerBoxCox = function(fcast, fcast_bc){
      if (super$rmseAccuracy(fcast) <= super$rmseAccuracy(fcast_bc)) {
        super$setFcasted(fcast)
      } else {
        super$setFcasted(fcast_bc)
        super$setBoxCoxApplied(T)
      }
    },
    analyseResiduals = function(df){
      residuals <- zoo::na.approx(super$getFcasted()$residuals)
      super$testResidualsRandomnessBox(residuals, df)
      super$testResidualsNormality(residuals)
    },
    considerBootstrap = function(fcast, fcast_bc){
      if (super$areResidualsNotNormal() & !super$areResidualsNotRandom()) {
        if (super$isBoxCoxApplied()) {
          super$setFcasted(do.call(fcast_bc, list()))
        } else {
          super$setFcasted(do.call(fcast, list()))
        }
        super$setBootstrapNotUsed(F)
      }
    }
  )
)
