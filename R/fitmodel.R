#' Class FitModel
#'
#' Represents models that need to be fitted before forecasting.
#'
#' @section Direct Superclass:{
#' \itemize{
#' \item \link{NVDModel}
#' }
#' }
#'
#' @section Direct Subclasses:{
#' \itemize{
#' \item \link{ARFIMAModel},
#' \item \link{ARIMAModel},
#' \item \link{BaggedETSModel},
#' \item \link{ETSModel},
#' \item \link{NNARModel},
#' \item \link{StructTSModel},
#' \item \link{TBATSModel},
#' \item \link{TSLinearModel},
#' }
#' }
#'
#' @section Private attributes:{
#' \itemize{
#'   \item \strong{fit} the attribute to store the fitted model's object (default NA)
#' }
#' }
#'
#'
#' @section Public methods:{
#' \itemize{
#'   \item \strong{getFitted()} returns private attribute \code{fit}
#'   \item \strong{setFitted(fit_result)} sets \code{fit_result} as the value of private attribute \code{fit}
#'   \item \strong{setFittedFcasted(fit_function, test_randomness, fcast_function, ...)} calls the fitting function, the forecasting function, performs residuals analysis and generates forecast intervals from bootstrapped residuals for ARIMA and ETS models when \code{!super$isPiIgnored() & super$areResidualsNotNormal() & !super$areResidualsNotRandom()}
#'   \item \strong{executeUseModel(fit_function, fcast_function, fcast_period, ...)} calls the fitting function, which uses the new training set (obtained by calling \link{NVDModel}'s method \code{getMergedTrainTestSet}), then calls the forecasting function which uses generating forecasting intervals from bootstrapped residuals when \code{!super$isBootstrapNotUsed() = TRUE}
#' }
#' }
#'
#' @keywords internal
#'
#' @importFrom R6 R6Class
#' @name FitModel
#' @export
#'
NULL
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
      if (!super$isPiIgnored() & super$areResidualsNotNormal() &
          !super$areResidualsNotRandom()) {
        super$setFcasted(do.call(fcast_function,
                                 list(self$getFitted(),
                                      bootstrap = T,
                                      fcast_period = super$getFcastPeriod(),
                                      ...)))
        compare <- nvdr::NameComparer$new(super$getMethod())
        if (compare$isName("arima") | compare$isName("ets")) {
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
