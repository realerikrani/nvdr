#' NVD vulnerabilities XML parser
#'
#' A function that goes into \code{XML::xmlEventParse} as \code{branches}
#' argument to parse NVD entries' nodes marked as \code{entry}.
#'
#' @keywords internal
#'
#' @return A list containing a node-parsing-function and a constructor, where
#' constructor returns a \code{data.table::data.table} of entries from XML.
#' @export
#'
#' @examples
#' \dontrun{
#'   parser <- nvd_entry_parser()
#'   XML::xmlEventParse(file = "test.csv", handlers = list(), branches = parser)
#'   parser$get_vulnerabilities()
#' }
nvd_entry_parser <- function() {
  id <- date <- cwes <- access_vector <- access_complexity <- authentication <-
    confidentiality_impact <- integrity_impact <- availability_impact <-
    character()
  score <- numeric()
  rejected <- logical()
  entry <- function(node) {
    id <<- c(id, XML::xmlGetAttr(node, "id"))
    date <<- c(date, get_xml_date(node))
    xml_cvss <- get_xml_cvss(node)
    score <<- c(score, xml_cvss[[1]])
    access_vector <<- c(access_vector, xml_cvss[[2]])
    access_complexity <<- c(access_complexity, xml_cvss[[3]])
    authentication <<- c(authentication, xml_cvss[[4]])
    confidentiality_impact <<- c(confidentiality_impact, xml_cvss[[5]])
    integrity_impact <<- c(integrity_impact, xml_cvss[[6]])
    availability_impact <<- c(availability_impact, xml_cvss[[7]])
    rejected <<- c(rejected, get_xml_reject(node))
    cwes <<- c(cwes, get_xml_cwes(node))
  }
  constructor <- function(){
    data.table::data.table(
      cve_id = id, cve_rejected = rejected, published = date, cwe = cwes,
      cvss_score = score, cvss_av = access_vector, cvss_ac = access_complexity,
      cvss_au = authentication, cvss_c = confidentiality_impact,
      cvss_i = integrity_impact, cvss_a = availability_impact)
    }
  list(entry = entry, get_vulnerabilities = constructor)
}

#' NVD entry extractor from one XML file
#'
#' A helper function for \code{get_nvd_entries}, uses \code{XML::xmlEventParse}.
#'
#' @keywords internal
#' @param file An XML Version 2.0 file from
#' \url{https://nvd.nist.gov/vuln/data-feeds#CVE_FEED}
#'
#' @return A data.table containing NVD entries extracted from a file.
#' @export
#'
#' @examples
#' \dontrun{get_nvd_from_file("nvdcve-2.0-2013.xml")}
get_nvd_from_file <- function(file){
  parser <- nvd_entry_parser()
  XML::xmlEventParse(file = file, handlers = list(), branches = parser)
  parser$get_vulnerabilities()
}

#' NVD entry extractor from many XML files
#'
#' A helper function for \code{get_nvd_entries}.
#'
#' @keywords internal
#' @param files XML Version 2.0 files from
#' \url{https://nvd.nist.gov/vuln/data-feeds#CVE_FEED}
#'
#' @return A data.table containing NVD entries extracted from files.
#' @export
#'
#' @examples
#' \dontrun{get_nvd_entries(c("nvdcve-2.0-2013.xml","nvdcve-2.0-2015.xml"))}
get_nvd_from_files <- function(files){
  data.table::rbindlist(lapply(files, get_nvd_from_file))
}

#' NVD entry extractor from XML file(s)
#'
#' Takes one or many files as arguments, processes these and returns a
#' data.table containing entries, each having an CVE id,
#' CWE types, logical value indicating whether the entry has been rejected,
#' publication date, CVSS base score, CVSS vectors (access vector,
#' access complexity, authentication, confidentiality impact, integrity impact,
#' availability impact)
#'
#' @param files One or many XML Version 2.0 files from
#' \url{https://nvd.nist.gov/vuln/data-feeds#CVE_FEED}
#'
#' @return A data.table containing NVD entries extracted from (a) file(s).
#' @export
#'
#' @examples
#' \dontrun{
#' get_nvd_entries(c("nvdcve-2.0-2013.xml","nvdcve-2.0-2015.xml"))
#' get_nvd_from_file("nvdcve-2.0-2013.xml")
#' }
get_nvd_entries <- function(files){
  if (length(files) == 1) {
    get_nvd_from_file(files)
  }else{
    get_nvd_from_files(files)
  }
}
