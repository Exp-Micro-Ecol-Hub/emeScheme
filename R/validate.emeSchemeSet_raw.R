#' @export
#'
#' @importFrom taxize gnr_resolve
#' @importFrom magrittr %>% %<>% extract2
#' @importFrom utils browseURL glob2rx read.csv
#' @importFrom rmarkdown render
#' @importFrom digest digest
#'
validate.emeSchemeSet_raw <- function(
  x,
  path = ".",
  validateData = TRUE,
  errorIfStructFalse = TRUE
) {


  # Define result structure of class emeScheme_validation ----------------------

  result <- new_emeScheme_validation()
  result$description <- paste(
    "The result of the overall validation of the data."
  )
  result$descriptionDetails <- paste(
    "The details contain the different validations of the metadata as a hierarchical list.",
    "errors propagate towards the root, i.e., if the 'worst' is a 'warning' in a validation in `details`",
    "the error here will be a 'warning' as well."
  )

  # Validate structure ------------------------------------------------------

  result$structure <- dmdScheme:::validateStructure( x )
  if (result$structure$error != 0 & errorIfStructFalse) {
    message(result$structure$details)
    stop("Structure of the object to be evaluated is wrong. See the info above for details.")
  }


  # Validata data -----------------------------------------------------------

  if ((result$structure$error == 0) & validateData) {
    xconv <- suppressWarnings( as_dmdScheme(x, keepData = TRUE, convertTypes = TRUE,  verbose = FALSE, warnToError = FALSE) )
    xraw  <-                   as_dmdScheme(x, keepData = TRUE, convertTypes = FALSE, verbose = FALSE, warnToError = FALSE)

    result$Experiment <- validateExperiment(x, xraw, xconv)
    result$Species <- validateSpecies(x, xraw, xconv)
    result$Treatment <- validateTreatment(x, xraw, xconv)
    result$Measurement <- validateMeasurement(x, xraw, xconv)
    result$DataExtraction <- validateDataExtraction(x, xraw, xconv)
    result$DataFileMetaData <- validateDataFileMetaData(x, xraw, xconv, path)
#    result$DataFiles <- validateDataFiles(x, xraw, xconv, path)
  }

  # Set overall error -------------------------------------------------------

  result$error <- max(valErr_extract(result), na.rm = FALSE)
  if (is.na(result$error)) {
    # result$error <- 3
  }
  result$header <- valErr_TextErrCol("Overall MetaData", result$error)

  # Return result -----------------------------------------------------------

  return(result)
}

# new emeScheme_validation object -----------------------------------------

new_emeScheme_validation <- function() {
  result <- list(
    error = NA,
    details = NA,
    header = "To Be Added",
    description = "To Be Added",
    descriptionDetails = "To Be Added",
    comment = ""
  )
  class(result) <- append( c("emeScheme_validation", "dmdScheme_validation"), class(result) )
  return(result)
}

