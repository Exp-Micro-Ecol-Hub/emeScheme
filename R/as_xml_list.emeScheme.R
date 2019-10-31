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
