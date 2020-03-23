#' Extract metadasta from one dataFile from an \code{emeScheme} object
#'
#' One \code{emeScheme} object can contain metadata for multiple dataFiles. For
#' archiving, these should be split into single \code{emeScheme} objects, one
#' for each \code{DataFileNameMetaData$dataFileName}. This function extracts the
#' metadasta from one dataFile in an \code{emeScheme} object and returns an
#' \code{emeScheme} object.
#'
#' The filtering is done as followed:
#' \describe{
#'   \item{DataFileMetaDFata}{\code{DataFileMetaData$dataFileName == dataFile}}
#'   \item{Treatment}{\code{Treatment$parameter \%in\% DataFileMetaData$mappingColumn}
#'     and \code{DataFileMetaData$columnData == "Treatment"} }
#'   \item{Measurement}{\code{Measurement$name \%in\% DataFileMetaData$mappingColumn}
#'     and \code{DataFileMetaData$columnData == "Measurement"} }
#' }
#' @param dataFile name of dataFile whose metadata will be extracted from
#'   \code{x}. Has to be an exact match, no wildcards are expanded.
#' @param x \code{emeScheme} object from which to extract the metadata
#'
#' @return An \code{emeScheme} object containing metadata for the data file \code{dataFileName}
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom methods is
#' @export
#'
#' @examples
#' emeScheme_extract("smell.csv", dmdScheme_example())
#' ## returns the emeScheme data for the data file 'smell.csv'
#'
#' emeScheme_extract("DoesNotExist", dmdScheme_example())
#' ## returns an empty emeScheme
emeScheme_extract <- function(
  dataFile,
  x
) {

# Argument check ----------------------------------------------------------

  if (!is(dataFile, "character")) {
    stop("dataFile has to be a string")
  }
  if (length(dataFile) > 1) {
    stop("dataFile has to be of length 1")
  }
  if (!is(x, "emeSchemeSet")) {
    stop("x has to be an object of type emeSchemeSet")
  }

# Get property name of x --------------------------------------------------

  propertyName <- attr(x, "propertyName")

# DataFileMetaData: keep only rows in which dataFileName == dataFile ------

  attrs <- attributes(x$DataFileMetaData)
  x$DataFileMetaData <- subset(x$DataFileMetaData, x$DataFileMetaData == dataFile)
  rownames(x$DataFileMetaData) <- NULL
  if (nrow(x$DataFileMetaData) > 0) {
    attrs[["row.names"]] <- 1:nrow(x$DataFileMetaData)
  } else {
    attrs[["row.names"]] <- NULL
  }

  attributes(x$DataFileMetaData) <- attrs

# Treatment: Only keep treatmentID which are still in DataFileMetaData ------

  selTreatmentID <- subset(
    x      = x$DataFileMetaData$mappingColumn,
    subset = (x$DataFileMetaData$columnData == "Treatment") | (x$DataFileMetaData$columnData == "Species")
  )
  attrs <- attributes(x$Treatment)
  x$Treatment <- subset(x$Treatment, x$Treatment$treatmentID %in% selTreatmentID)
  rownames(x$Treatment) <- NULL
  if (nrow(x$Treatment) > 0) {
    attrs[["row.names"]] <- 1:nrow(x$Treatment)
  } else {
    attrs[["row.names"]] <- NULL
  }

  attributes(x$Treatment) <- attrs

# Measurement: Only keep measurementID which are still in DataFileMetaData ---------

  selMeasurementID <- subset(
    x      = x$DataFileMetaData$mappingColumn,
    subset = (x$DataFileMetaData$columnData == "Measurement")
  )

  attrs <- attributes(x$Measurement)
  x$Measurement <- subset(x$Measurement, x$Measurement$measurementID %in% selMeasurementID)
  rownames(x$Measurement) <- NULL
  if (nrow(x$Measurement) > 0) {
    attrs[["row.names"]] <- 1:nrow(x$Measurement)
  } else {
    attrs[["row.names"]] <- NULL
  }

  attributes(x$Measurement) <- attrs

# ExtractionMethod: Only keep extractionMethodID which are still in DataFileMetaData --------

  selDataExtractionID <- unique(x$Measurement$dataExtractionID)

  attrs <- attributes(x$DataExtraction)
  x$DataExtraction <- subset(x$DataExtraction, x$DataExtraction$dataExtractionID %in% selDataExtractionID)
  rownames(x$DataExtraction) <- NULL
  if (nrow(x$DataExtraction) > 0) {
    attrs[["row.names"]] <- 1:nrow(x$DataExtraction)
  } else {
    attrs[["row.names"]] <- NULL
  }
  attributes(x$DataExtraction) <- attrs


# Species: Only keep speciesID which are still in treatmentID ----------------------------------

  selTreatmentID <- subset(
    x      = x$DataFileMetaData$mappingColumn,
    subset = (x$DataFileMetaData$columnData == "Species")
  )

  selSpeciesID <- subset( x$Treatment, subset = x$Treatment$treatmentID %in% selTreatmentID, select = "treatmentLevel") %>%
    unlist() %>%
    strsplit(",") %>%
    unlist() %>%
    trimws()

  attrs <- attributes(x$Species)
  x$Species <- subset(x$Species, x$Species$speciesID %in% selSpeciesID)
  rownames(x$Species) <- NULL
  if (nrow(x$Species) > 0) {
    attrs[["row.names"]] <- 1:nrow(x$Species)
  } else {
    attrs[["row.names"]] <- NULL
  }
  attributes(x$Species) <- attrs

# Set property name -------------------------------------------------------

  attr(x, "propertyName") <- paste(dataFile, propertyName, sep = "_")

# Return ------------------------------------------------------------------

  return(x)
}
