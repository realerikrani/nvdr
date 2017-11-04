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
    }
  ),

  private = list(
    mean_model = NA,
    drift_model = NA,
    naive_model = NA,
    snaive_model = NA,

    buildModels = function(){
      cwe_ts <- ts(c(super$getTrainingSet(),super$getTestSet()),
                   start = start(super$getTrainingSet()),
                   frequency = frequency(super$getTrainingSet()))
      cwe <- super$getCWE()
      private$mean_model <- MeanModel$new(cwe, cwe_ts)
      private$drift_model <- DriftModel$new(cwe, cwe_ts)
      private$naive_model <- NaiveModel$new(cwe, cwe_ts)
      private$snaive_model <- SeasonalNaiveModel$new(cwe, cwe_ts)
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
      mean_assessment <- self$getMeanModel()$getAssessment()
      drift_assessment <- self$getDriftModel()$getAssessment()
      naive_assessment <- self$getNaiveModel()$getAssessment()
      snaive_assessment <- self$getSNaiveModel()$getAssessment()
      metrics <-
        data.table::rbindlist(lapply(names(mean_assessment), function(metric){
          list(mean_assessment[metric],drift_assessment[metric],
               naive_assessment[metric],snaive_assessment[metric])
        }
        ))
      colnames(metrics) <- c("mean", "drift", "naive", "snaive")
      mins <- apply(metrics, 1, function(m){
        ifelse(all(is.infinite(m)),"",colnames(metrics)[which.min(m)])
      })
      best <- names(sort(summary(as.factor(mins)), decreasing = TRUE)[1])
      switch(
        best,
        "mean" = {
          private$pickedModelSetter(m = self$getMeanModel())
          },
        "drift" = {
          private$pickedModelSetter(m = self$getDriftModel())
          },
        "naive" = {
          private$pickedModelSetter(m = self$getNaiveModel())
          },
        {
          private$pickedModelSetter(m = self$getSNaiveModel())
          }
      )
      },

    pickedModelSetter = function(m){
      super$setFcasted(m$getFcasted())
      super$setResidualsNotRandom(m$areResidualsNotRandom())
      super$setResidualsNotNormal(m$areResidualsNotNormal())
    }
    )
  )
