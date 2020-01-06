#' Convert \code{emeSchemeSet} to an \code{xml_list} object
#'
#' @param x object to be converted.
#' @param output specifies the content and format of the exported xml.
#' \describe{
#'   \item{"metadata" : }{export of the metadata only with no format attributes}
#'   \item{"complete" : }{export tof the complete sheme, i.e. "metadata" plus the scheme definition. This is a self contained format which contains all attributes.}
#' }
#' @param ... additional arguments for methods
#'
#' @rdname as_xml_list
#' @export
#'
as_xml_list.emeSchemeSet <- function(
  x,
  output = "metadata",
  ...
) {
  # Extract DataFileMetaData$dataFileNames ----------------------------------

  dataFileName <- unique(x$DataFileMetaData$dataFileName)
  names(dataFileName) <- dataFileName

  # Extract for each unique dataFileName ------------------------------------

  splitted <- lapply(
    dataFileName,
    emeScheme_extract,
    x = x
  )

  # Convert to list of xml_document -----------------------------------------

  xml_list <- lapply(
    splitted,
    dmdScheme::as_xml,
    output = output,
    ... = ...
  )

  # Return xml --------------------------------------------------------------

  return(xml_list)
}
