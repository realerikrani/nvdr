findBestColumn <- function(metrics) {
  mins <- apply(metrics, 1, function(m){
    ifelse(all(is.infinite(m)), "", colnames(metrics)[which.min(m)])
  })
  best <- names(sort(summary(as.factor(mins)), decreasing = T)[1])
  best
}

NameComparer <- R6::R6Class(
  "NameComparer",
  public = list(
    initialize = function(method) {
      private$method_name <- method
    },
    isName = function(name){
      private$name(name)
    }
    ),
  private = list(
    method_name = NA,
    name = function(x){
      grepl(paste0("\\b", x, "\\b"), private$method_name, ignore.case = T)
    }
  )
)
