FitModel <- R6::R6Class(
  "FitModel",
  inherit = NVDModel,

  public = list(
    getFitted = function(){
      private$fit
    }
  ),

  private = list(
    fit = NA
  )
)