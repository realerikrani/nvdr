#' NVD Data Includer
#'
#' The \code{nvdr} package contains processed ready-to-use NVD data.
#'
#' @return Binary data (in .rda format) in folder \code{/data}.
#'
#' @examples
#' \dontrun{xml_to_binary()}
xml_to_binary <- function(){
  years <- c("11", "12", "13", "14", "15", "16")
  nvd_raw_files <- unlist(lapply(years, function(year){
    paste0("data-raw/nvdcve-2.0-20", year, ".xml")
  }))
  nvd <- get_nvd_entries(nvd_raw_files)
  devtools::use_data(nvd)
}
