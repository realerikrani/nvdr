FitModel <- R6::R6Class(
  "FitModel",
  inherit = NVDModel,

  public = list(
    getFitted = function(){
      private$fit
    },
    setFitted = function(fit_result){
      private$fit <- fit_result
    },
    setFittedFcasted = function(fit_function, test_randomness, fcast_function,
                                ...){
      self$setFitted(do.call(fit_function, list(super$getTrainingSet(), ...)))
      residuals <- super$getResiduals(self$getFitted())
      do.call(test_randomness, list(residuals))
      super$setResidualsNotNormal(super$testResidualsNormality(residuals))
      if (super$areResidualsNotNormal() & !super$areResidualsNotRandom() &
          !super$isPiIgnored()) {
        super$setFcasted(do.call(fcast_function,
                                 list(self$getFitted(),
                                      bootstrap = T,
                                      fcast_period = super$getFcastPeriod(),
                                      ...)))
        compare <- nvdr::NameComparer$new(super$getMethod())
        if (!(compare$isName("linear") & compare$isName("regression"))){
          super$setBootstrapNotUsed(F)
        }
      } else {
        super$setFcasted(do.call(fcast_function,
                                 list(self$getFitted(),
                                      fcast_period = super$getFcastPeriod(),
                                      ...)))
      }
    },
    executeUseModel = function(fit_function, fcast_function,
                               fcast_period, ...) {
      ft <- do.call(fit_function, list(super$getMergedTrainTestSet(), ...))
      fc <- do.call(fcast_function,
                    list(ft, fcast_period = fcast_period,
                         bootstrap = !super$isBootstrapNotUsed(), ...))
      return(fc)
    }
  ),

  private = list(
    fit = NA
  )
)
