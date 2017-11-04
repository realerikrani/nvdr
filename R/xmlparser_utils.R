#' CWE extractor from XML
#'
#' Extracts CWE types from XML node. If some NVD entry has
#' multiple types, then these are handled as one string connceted by "|". If
#' the types are missing, then NA is returned.
#' @keywords internal
#'
#' @param node An XMLNode \code{entry}.
#'
#' @return The character of CWEs from XML.
#' @export
#'
#' @examples
#' \dontrun{get_xml_cwes(node)}
#'
get_xml_cwes <- function(node) {
  if (!is.null(XML::xmlChildren(node)[[c("vuln:cwe")]])) {
    children <- as.list(XML::xmlChildren(node))
    cwe_indices <- which(names(as.list(XML::xmlChildren(node))) %in% "vuln:cwe")
    xml_cwes <- paste(unlist(lapply(cwe_indices, function(i) {
      XML::xmlGetAttr(children[[i]], "id")
    }
    )), collapse = "|")
  } else {
    xml_cwes <- NA
  }
}


#' CVSS base metrics node extractor from XML
#'
#' Gets XMLNode \code{cvss:base_metrics} which is
#' inside XMLNode \code{vuln:cvss}.
#' @keywords internal
#'
#' @param node An XMLNode \code{entry}.
#'
#' @return XMLNode \code{cvss:base_metrics}.
#' @export
#'
#' @examples
#' \dontrun{get_xml_base_metrics(node)}
get_xml_base_metrics <- function(node) {
  XML::xmlChildren(
    XML::xmlChildren(node)[[c("vuln:cvss")]]
  )[[c("cvss:base_metrics")]]
}


#' Base metric's submetric extractor from XML
#'
#' Gets specific submetricts from \code{cvss:base_metrics}.
#' @keywords internal
#'
#' @param base_metrics Outcome from \code{get_xml_base_metrics}.
#' @param metric A string specifing the metric.
#'
#' @return A base metric value as a character.
#' @export
#'
#' @examples
#' \dontrun{get_xml_metrics(base_metrics, "score")}
get_xml_metrics <- function(base_metrics, metric) {
  XML::xmlValue(XML::xmlChildren(base_metrics)[[c(paste0("cvss:", metric))]])
}


#' CVSS metrics extractor from XML
#'
#' Gets CVSS metrics from XML.
#' @keywords internal
#'
#' @param node An XMLNode \code{entry}.
#'
#' @return A list with CVSS metrics as characters.
#'
#' @examples
#' \dontrun{get_xml_cvss(node)}
get_xml_cvss <- function(node) {
  if (!is.null(XML::xmlChildren(node)[[c("vuln:cvss")]])) {
    base_metrics <- get_xml_base_metrics(node)
    xml_score <- get_xml_metrics(base_metrics, "score")
    xml_avector <- get_xml_metrics(base_metrics, "access-vector")
    xml_acomplexity <- get_xml_metrics(base_metrics, "access-complexity")
    xml_authentication <- get_xml_metrics(base_metrics, "authentication")
    xml_cimpact <- get_xml_metrics(base_metrics, "confidentiality-impact")
    xml_iimpact <- get_xml_metrics(base_metrics, "integrity-impact")
    xml_aimpact <- get_xml_metrics(base_metrics, "availability-impact")
  } else {
    xml_score <- xml_avector <- xml_acomplexity <- xml_authentication <-
      xml_cimpact <- xml_iimpact <- xml_aimpact <- NA
  }
  list(xml_score, xml_avector, xml_acomplexity, xml_authentication,
       xml_cimpact, xml_iimpact, xml_aimpact)
}


#' Rejected entry detector from XML
#'
#' Checks the XMLNode \code{entry}'s child \code{vuln:summary}. If the rejection
#' character is found, then TRUE is returned.
#'
#' @keywords internal
#' @param node An XMLNode \code{entry}.
#'
#' @return Logical value, TRUE when the entry is rejected.
#' @export
#'
#' @examples
#' \dontrun{get_xml_reject(node)}
get_xml_reject <- function(node) {
  grepl("** REJECT **",
        XML::xmlValue(XML::xmlChildren(node)[[c("vuln:summary")]]),
        fixed = TRUE)
}


#' Published date extractor from XML
#'
#' Gets the datetime that shows when NVD entry was published and extracts a
#' date.
#'
#' @keywords internal
#' @param node An XMLNode \code{entry}.
#'
#' @return A character of date containing year, month and day.
#' @export
#'
#' @examples
#' \dontrun{get_xml_date(node)}
get_xml_date <- function(node) {
  gsub("T.*", "",
       XML::xmlValue(
         XML::xmlChildren(node)[[c("vuln:published-datetime")]])
  )
}
