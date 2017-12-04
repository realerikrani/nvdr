BenchmarkModel <- R6::R6Class(
  "BenchmarkModel",
  inherit = NVDModel,

  public = list(
    getMeanModel = function(){
      private$mean_model
    },
    getDriftModel = function(){
      private$drift_model
    },
    getNaiveModel = function(){
      private$naive_model
    },
    getSNaiveModel = function(){
      private$snaive_model
    },
    buildModel = function(){
      private$buildModels()
      private$assessModels()
      private$pickBestModel()
    },
    useModel = function(fcast_period, residuals_check = T){
      if (length(self$getMeanModel()) > 1) {
        self$getMeanModel()$useModel(fcast_period, residuals_check)
      } else if (length(self$getDriftModel()) > 1) {
        self$getDriftModel()$useModel(fcast_period, residuals_check)
      } else if (length(self$getNaiveModel()) > 1) {
        self$getNaiveModel()$useModel(fcast_period, residuals_check)
      } else {
        self$getSNaiveModel()$useModel(fcast_period, residuals_check)
      }
    }
  ),

  private = list(
    mean_model = NA,
    drift_model = NA,
    naive_model = NA,
    snaive_model = NA,

    buildModels = function(){
      cwe_ts <- ts(c(super$getTrainingSet(), super$getTestSet()),
                   start = start(super$getTrainingSet()),
                   frequency = frequency(super$getTrainingSet()))
      cwe <- super$getCWE()
      start_year <- super$getStartYear()
      end_year <- super$getEndYear()
      end_month <- super$getFcastPeriod()
      private$mean_model <- MeanModel$new(cwe, cwe_ts, start_year, end_year,
                                          end_month)
      private$drift_model <- DriftModel$new(cwe, cwe_ts, start_year, end_year,
                                            end_month)
      private$naive_model <- NaiveModel$new(cwe, cwe_ts, start_year, end_year,
                                            end_month)
      private$snaive_model <- SeasonalNaiveModel$new(cwe, cwe_ts, start_year,
                                                     end_year, end_month)
      self$getMeanModel()$buildModel()
      self$getDriftModel()$buildModel()
      self$getNaiveModel()$buildModel()
      self$getSNaiveModel()$buildModel()
    },
    assessModels = function(){
      self$getMeanModel()$assessModel()
      self$getDriftModel()$assessModel()
      self$getNaiveModel()$assessModel()
      self$getSNaiveModel()$assessModel()
    },
    pickBestModel = function(){
      metrics <- cbind("mean" = self$getMeanModel()$getAssessment(),
                       "drift" = self$getDriftModel()$getAssessment(),
                       "naive" = self$getNaiveModel()$getAssessment(),
                       "snaive" = self$getSNaiveModel()$getAssessment())
      best <- nvdr::findBestColumn(metrics)
      switch(
        best,
        "mean" = {
          private$pickedModelSetter(m = self$getMeanModel())
          private$drift_model <- private$naive_model <-
            private$snaive_model <- NA
          },
        "drift" = {
          private$pickedModelSetter(m = self$getDriftModel())
          private$mean_model <- private$naive_model <-
            private$snaive_model <- NA
          },
        "naive" = {
          private$pickedModelSetter(m = self$getNaiveModel())
          private$drift_model <- private$mean_model <-
            private$snaive_model <- NA
          },
          {
          private$pickedModelSetter(m = self$getSNaiveModel())
          private$drift_model <- private$naive_model <-
            private$mean_model <- NA
          }
      )
      },

    pickedModelSetter = function(m){
      super$setFcasted(m$getFcasted())
      super$setResidualsNotRandom(m$areResidualsNotRandom())
      super$setResidualsNotNormal(m$areResidualsNotNormal())
      super$setBootstrapNotUsed(m$isBootstrapNotUsed())
      super$setBoxCoxApplied(m$isBoxCoxApplied())
    }
    )
  )
