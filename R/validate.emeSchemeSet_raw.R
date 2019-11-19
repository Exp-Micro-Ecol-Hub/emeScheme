#' @export
#'
#' @importFrom taxize gnr_resolve
#' @importFrom magrittr %>% %<>% extract2
#' @importFrom dplyr filter select
#' @importFrom utils browseURL glob2rx read.csv
#' @importFrom rmarkdown render
#' @importFrom tibble tibble as_tibble
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

  result$structure <- validateStructure( x )
  if (result$structure$error != 0 & errorIfStructFalse) {
    message(result$structure$details)
    stop("Structure of the object to be evaluated is wrong. See the info above for details.")
  }


  # Validata data -----------------------------------------------------------

  if ((result$structure$error == 0) & validateData){
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

# Internal validation helper functions ------------------------------------

validateTypes <- function(sraw, sconv) {
  result <- new_emeScheme_validation()
  ##
  result$header <- "conversion of values into specified type lossless possible"
  result$description <- paste(
    "Test if the metadata entered follows the type for the column, i.e. integer, characterd, ....",
    "The validation is done by verifying if the column can be losslessly converted from character to the columnb type specified.",
    "the value NA is allowed in all column types, empty cells should be avoided."
  )
  result$descriptionDetails <- paste(
    "The details are a table of the same dimension as the input (green) area of the meatadata sheet.",
    "The following values are possible:\n",
    "\n",
    "   FALSE: If the cell contains an error, i.e. can not be losslessly converted.\n",
    "   TRUE : If the cell can be losslessly converted and is OK.\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE values will result in an ERROR."
  )
  ##
  result$details <- as.data.frame(sraw, stringsAsFactors = FALSE)
  if ( nrow(sraw) != 0 ) {
    t <- sraw == sconv
    na <- is.na(t)
    t[na] <- TRUE
    result$details[t] <- TRUE
    result$details[!t] <- paste( result$details[!t], "!=", as.data.frame(sconv)[!t])
    result$details[na] <- NA
  } else {
    result$details[1,] <- rep(NA, ncol(result$details))
  }
  result$details <- tibble::as_tibble(result$details, .name_repair = "unique")
  ##
  result$error = ifelse(
    all(result$details == TRUE, na.rm = TRUE),
    0,
    3
  )
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return( result )
}

validateSuggestedValues <- function(sraw) {
  result <- new_emeScheme_validation()
  ##
  result$header <- "values in suggestedValues"
  result$description <- paste(
    "Test if the metadata entered is ion the suggestedValues list.",
    "The value NA is allowed in all column types, empty cells should be avoided."
  )
  result$descriptionDetails <- paste(
    "The details are a table of the same dimension as the input (green) area of the meatadata sheet.",
    "The following values are possible:\n",
    "\n",
    "   FALSE: If the cell value is not contained in the suggestedValues list.\n",
    "   TRUE : If the cell value is contained in the suggestedValues list.\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE values will result in a WARNING."
  )
  ##
  result$details <- tibble::as_tibble(sraw, .name_repair = "unique")
  sugVal <- strsplit(attr(sraw, "suggestedValues"), ",")
  result$details <- result$details[,!is.na(sugVal)]
  sugVal <- sugVal[!is.na(sugVal)]
  ##
  for (colN in 1:ncol(result$details)) {
    v <- c( trimws(sugVal[[colN]]), "NA", NA, "" )
    for (rowN in 1:nrow(result$details)) {
      al <- result$details[rowN, colN] %in% v
      # al <- ifelse(
      #   al,
      #   TRUE,
      #   paste0("'", result$details[rowN, colN], "' not in suggested Values!")
      # )
      result$details[rowN, colN] <- al
    }
  }
  ##
  result$error = ifelse(
    all(result$details == TRUE, na.rm = TRUE),
    0,
    1
  )
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return( result )
}

validateAllowedValues <- function(sraw) {
  result <- new_emeScheme_validation()
  ##
  result$header <- "values in allowedValues"
  result$description <- paste(
    "Test if the metadata entered is ion the allowedValues list.",
    "The value NA is allowed in all column types, empty cells should be avoided."
  )
  result$descriptionDetails <- paste(
    "The details are a table of the same dimension as the input (green) area of the meatadata sheet.",
    "The following values are possible:\n",
    "\n",
    "   FALSE: If the cell value is not contained in the allowedValues list.\n",
    "   TRUE : If the cell value is contained in the allowedValues list.\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE values will result in an ERROR."
  )
  ##
  result$details <- tibble::as_tibble(sraw, .name_repair = "unique")
  allVal <- strsplit(attr(sraw, "allowedValues"), ",")
  result$details <- result$details[,!is.na(allVal)]
  allVal <- allVal[!is.na(allVal)]
  ##
  for (colN in 1:ncol(result$details)) {
    v <- c( trimws(allVal[[colN]]), "NA", NA, "" )
    for (rowN in 1:nrow(result$details)) {
      al <- result$details[rowN, colN] %in% v
      # al <- ifelse(
      #   al,
      #   TRUE,
      #   paste0("'", result$details[rowN, colN], "' not in allowed Values!")
      # )
      result$details[rowN, colN] <- al
    }
  }
  ##
  result$error = ifelse( all(result$details == TRUE,na.rm = TRUE), 0, 3)
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return( result )
}

valitdateSpeciesIDUnique <- function(x){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Test if `speciesID` is unique"
  result$description <- paste(
    "Test if the speciesID is unique in this metadata set.",
    "The functions returns `TRUE` if all speciesID are unique."
  )
  result$descriptionDetails <- paste(
    "Returns a named vector, with the following possible values:\n",
    "\n",
    "   TRUE  : the value in `speciesID` is unique\n",
    "   FALSE : the value in `speciesID` is not unique\n",
    "\n",
    "One or more FALSE or a missing value will result in an ERROR."
  )
  ##
  result$details <- data.frame(
    speciesID = x$Species$speciesID,
    isOK =  !duplicated(x$Species$speciesID),
    stringsAsFactors = FALSE
  )
  ##
  result$error <-  ifelse(
    all(result$details$isOK),
    0,
    3
  )
  if (any(is.na(x$Species$speciesID))) {
    result$error <- 3
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateSpeciesNames <- function(x) {
  result <- new_emeScheme_validation()
  ##
  result$header <- "Test if the scientific name is correct"
  result$description <- paste(
    "Test if the scientific name in the column `name` is correct.",
    "This uses the function `taxize::gnr_resolve()`",
    "The functions returns `TRUE` if all species have a score of >= 0.7."
  )
  result$descriptionDetails <- paste(
    "The details are a table as returned from the funcrtion `taxize::gnr_resolve()`.",
    "The columns are:\n",
    "\n",
    "   user_supplied_name : the name as in column `name`\n",
    "   submitted_name     : the actual name passed on to be resolved\n",
    "   matched_name       : the matched named\n",
    "   data_source_title  : the name of the data source which returned the best match\n",
    "   score              : a score from the match\n",
    "\n",
    "**Not matched species are not listed here!**."
  )
  ##
  result$details <- taxize::gnr_resolve(x$Species$name, best_match_only = TRUE)
  ##
  if (length(attr(result$details, "not_known")) > 0) {
    result$error <- 2
  } else if (min(result$details$score) < 0.7) {
    result$error <- 2
  } else {
    result$error <- 0
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateTreatmentMapping <- function(x){
  result <- new_emeScheme_validation()
  result$header <- "Test if treatmentID is in mappingColumn"
  result$description <- paste(
    "Test if the `treatmentID` is in the `DataFileMetaData$mappingColumn` column.",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If `treatmentID` contains missing values.\n",
    "   warning : If not all `treatmentID` are in the `DataFileMetaData$mappingColumn`.\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique `treatmentID`.",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the value is in `DataFileMetaData$mappingColumn`.\n",
    "   FALSE: If the value is not in `DataFileMetaData$mappingColumn`.\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$details <- data.frame(
    treatmentID = unique(x$Treatment$treatmentID),
    isOK =  unique(x$Treatment$treatmentID) %in% x$DataFileMetaData$mappingColumn,
    stringsAsFactors = FALSE
  )
  ##
  result$error <- ifelse(
    all(result$details$isOK),
    0,
    2
  )
  if (any(is.na(x$Treatment$treatmentID))) {
    result$error <- 3
  }
  #
  result$header <- valErr_TextErrCol(result)
  return(result)
}

validateMeasurementIDsUnique <- function(x){
  result <- new_emeScheme_validation()
  ##
  result$header <- "names unique"
  result$description <- paste(
    "Check if the names specified in `measurementID` are unique."
  )
  result$descriptionDetails <- paste(
    "Returns a named vector, with the following possible values:\n",
    "\n",
    "   TRUE  : the value in `speciesID` is unique\n",
    "   FALSE : the value in `speciesID` is not unique\n",
    "\n",
    "One or more FALSE or a missing value will result in an ERROR."
  )
  ##
  result$details <- data.frame(
    measurementID = x$Measurement$measurementID,
    isOK = !duplicated(x$Measurement$measurementID),
    stringsAsFactors = FALSE
  )
  ##
  result$error <-  ifelse(
    all(result$details$isOK),
    0,
    3
  )
  if (any(is.na(x$Measurement$measurementID))) {
    result$error <- 3
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateMeasurementMeasuredFrom <- function(x) {
  result <- new_emeScheme_validation()
  ##
  result$header <- "measuredFrom is 'raw', 'NA', NA or in name"
  result$description <- paste(
    "Test if the `measuredFrom` is in the `x$Measurement$measurementID` column, 'raw', 'NA', or `NA`.",
    "The `error` is 'error' if can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If the value is not in in the `x$Measurement$measurementID` column, 'raw', 'NA', or `NA`\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique `result$details",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the value is in `x$Measurement$measurementID` column, 'raw', 'NA', or `NA`.\n",
    "   FALSE: If the value is not in `x$Measurement$measurementID` column, 'raw', 'NA', or `NA`.\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$details <- data.frame(
    measurementID = x$Measurement$measurementID,
    isOK = x$Measurement$measuredFrom %in% c(x$Measurement$measurementID, "raw", "NA", NA),
    stringsAsFactors = FALSE
  )
  ##
  result$error = ifelse(
    all(result$details$isOK),
    0,
    3
  )
  if (any(is.na(x$Measurement$measurementID))) {
    result$error <- 3
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateMeasurementMapping <- function(x){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Test if `measurementID` is in mappingColumn"
  result$description <- paste(
    "Test if the `measurementID` is in the `DataFileMetaData$mappingColumn` column.",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If `measurementID` contains missing values.\n",
    "   warning : If not all `measurementID` are in the `DataFileMetaData$mappingColumn`.\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique `measurementID`",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the value is in `DataFileMetaData$mappingColumn`.\n",
    "   FALSE: If the value is not in `DataFileMetaData$mappingColumn.\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$details <- data.frame(
    measurementID = unique(x$Measurement$measurementID),
    isOK = unique(x$Measurement$measurementID) %in% x$DataFileMetaData$mappingColumn,
    stringsAsFactors = FALSE
  )
  ##
  result$error <- ifelse(
    all(result$details$isOK),
    0,
    2
  )
  if (any(is.na(x$Measurement$measurementID))) {
    result$error <- 3
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateMeasurementDataExtraction <- function(x) {
  result <- new_emeScheme_validation()
  ##
  result$header <- "dataExtractionID is 'none', 'NA', NA, or in DataExtraction$dataExtractionID"
  result$description <- paste(
    "Test if the `dataExtractionID` is in the `DataExtraction$dataExtractionID` column, 'none', 'NA', or `NA`.",
    "The `error` is 'error' if can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If the value is not in in the `DataExtraction$dataExtractionID` column, 'none', 'NA', or `NA`\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique `result$details",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the value is in `DataExtraction$dataExtractionID` column, 'none', 'NA', or `NA`\n",
    "   FALSE: If the value is not in `DataExtraction$dataExtractionID` column, 'none', 'NA', or `NA`\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE will result in an ERROR."
  )
  ##
  result$details <- data.frame(
    dataExtractionID = x$Measurement$dataExtractionID,
    isOK = x$Measurement$dataExtractionID %in% c(x$DataExtraction$dataExtractionID, "none", "NA", NA),
    stringsAsFactors = FALSE
  )
  #
  result$error <- ifelse(
    all(result$details$isOK),
    0,
    3
  )
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateDataExtractionIDLinked <- function(x) {
  result <- new_emeScheme_validation()
  result$header <- "name is in Measurement$dataExtractionID"
  result$description <- paste(
    "Test if the `dataExtractionID` is in the `Measurement$dataExtractionID` column.",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If not all `dataExtractionID` are in `Measurement$dataExtractionID`\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique `variable`",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the value is in `DataFileMetaData$mappingColumn`.\n",
    "   FALSE: If the value is not in `DataFileMetaData$mappingColumn.\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE will result in an ERROR."
  )
  ##
  result$details <- data.frame(
    dataExtractionID = x$DataExtraction$dataExtractionID,
    isOK = x$DataExtraction$dataExtractionID %in% x$Measurement$dataExtractionID,
    stringsAsFactors = FALSE
  )
  ##
  result$error <- ifelse(
    all(result$details$isOK),
    0,
    2
  )
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateDataFileMetaDataDataFileExists <- function(x, path) {
  result <- new_emeScheme_validation()
  ##
  result$header <- "`dataFile` exists in path"
  result$description <- paste(
    "Test if all `dataFile` exist in the given `path`.",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If not all `dataFile` exist in the given `path`\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique `variable`",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If `dataFile` exist in the given `path`\n",
    "   FALSE: If `dataFile` does not exist in the given `path`\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE or missing values will result in an ERROR."
  )
  ##
  fns <- unique(x$DataFileMetaData$dataFileName)
  result$details <- data.frame(
    dataFileName = fns,
    IsOK = fns %>% file.path(path, .) %>% file.exists(),
    stringsAsFactors = FALSE
  )
  ##
  result$error <- ifelse(
    all(result$details$IsOK),
    0,
    3
  )
  if (any(is.na(fns))) {
    result$error <- 3
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateDataFileMetaDataDateTimeSpecified <- function(x){
  result <- new_emeScheme_validation()
  result$header <- "Test if date time format has been specified if required"
  result$description <- paste(
    "Test if date time format has been specified in the `description` column when `type` is equal to 'datetime'.",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If not all `description` contain a date time format when `type` equals 'datetime'\n",
    "\n",
    "DOES NOT YET CHECK FOR THE VALIDITY OF THE FORMAT!!!!!"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per 'datetime' format row",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If `description` contains a value\n",
    "   FALSE: If `description` does not contain a value\n",
    "   NA   : empty cell\n",
    "\n",
    "One or more FALSE or missing values will result in an ERROR."
  )
  ##
  result$details <- x$DataFileMetaData %>%
    dplyr::select(.data$dataFileName, .data$columnName, .data$type, .data$description) %>%
    dplyr::filter(.data$type %in% c("date", "time", "datetime") )

  result$details$IsOK <- !is.na(result$details$description)
  ##
  result$error <- ifelse(
    all( result$details$IsOK ),
    0,
    3
  )
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateDataFileMetaDataMapping <- function(x) {
  ## if columnData == “Measurement”, mappingColumn has to be in Measurement$measurementID and
  ## if columnData == “Treatment”, mappingColumn has to be in Treatment$treatmentID and
  ## if columnData %in% c(ID, other), mapping column has to be "NA" or NA
  result <- new_emeScheme_validation()
  result$header <- "correct values in `mappingColumn`` in dependence on columnData"
  result$description <- paste(
    "Test if `mappingColumn` is found in the appropriate table.",
    "If `columnData == Species`,  `mappingColumn` has to be `NA` to result in TRUE!",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If not all `mappingColumn` are found in the appropriate columns\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per `mappingColumn` value format row",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If `mappingColumn` is found in apropriate table or NA\n",
    "   FALSE: If `mappingColumn` is not found in apropriate table\n",
    "\n",
    "One or more FALSE or missing values will result in an ERROR."
  )
  ##
  result$details <- x$DataFileMetaData$mappingColumn
  result$details[] <- NA
  #
  cd <- x$DataFileMetaData$columnData
  cd[is.na(cd)] <- "NA"
  #
  i <- cd == "Species"
  result$details[i] <- is.na(x$DataFileMetaData$mappingColumn[i])
  i <- cd == "Treatment"
  result$details[i] <- x$DataFileMetaData$mappingColumn[i] %in% x$Treatment$treatmentID
  i <- cd == "Measurement"
  result$details[i] <- x$DataFileMetaData$mappingColumn[i] %in% x$Measurement$measurementID
  i <- cd == "ID"
  result$details[i] <- x$DataFileMetaData$mappingColumn[i] %in% c("NA", NA)
  i <- cd == "other"
  result$details[i] <- x$DataFileMetaData$mappingColumn[i] %in% c("NA", NA)
  i <- cd == "NA"
  result$details[i] <- FALSE
  #

  result$details <- data.frame(
    columnData = x$DataFileMetaData$columnData,
    mappingColumn = x$DataFileMetaData$mappingColumn,
    IsOK = as.logical(result$details),
    stringsAsFactors = FALSE
  )
  ##
  result$error <- ifelse(
    all(result$details$IsOK),
    0,
    3
  )
  if (is.na(result$error)) {
    result$error <- 3
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

readColumnNamesFromDataFiles <- function(x, path) {
  dfcol <- file.path(path, x$DataFileMetaData$dataFileName) %>%
    unique() %>%
    lapply(
      function(x) {
        if (file.exists(x) & tools::file_ext(x) == "csv") {
          colnames(read.csv(x, nrows = 1))
        } else {
          NA
        }
      }
    )
  names(dfcol) <- x$DataFileMetaData$dataFileName %>% unique()
  return(dfcol)
}

validateDataFileMetaDataColumnNameInDataFile <- function(x, path) {
  result <- new_emeScheme_validation()
  result$header <- "`columnName` in column names found in column names in `dataFileName`"
  result$description <- paste(
    "Test if `columnName` is found in the `dataFileName`.",
    "Only comma separated data files (extension `csv`) will be checked!",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If not all `columnName` are found in column names in `dataFileName`\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per `columnName` value.",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If `columnName` is found in column names in `dataFileName` or NA\n",
    "   FALSE: If `columnName` is not found in column names in `dataFileName`\n",
    "\n",
    "One or more FALSE or missing values will result in an ERROR."
  )
  ##
  dfcol <- readColumnNamesFromDataFiles(x, path)
  result$details <- x$DataFileMetaData$columnName
  for (i in 1:nrow(x$DataFileMetaData)) {
    # result$details[i] <- x$DataFileMetaData$columnName[[i]] %in% dfcol[[x$DataFileMetaData$dataFileName[i]]]
    cn <- x$DataFileMetaData$columnName[[i]]
    if (grepl("!!!", cn)) {
      cn <- gsub("!!!", "", cn)
      cn <- glob2rx(cn)
    }
    result$details[i] <- any(
      grepl(
        cn,
        dfcol[[x$DataFileMetaData$dataFileName[i]]],
      )
    )
    if (is.na(result$details[i])) {
      result$details[i] <- FALSE
    }
    if (is.na(cn) ) {
      result$details[i] <- TRUE
    }

  }
  result$details <- data.frame(
      dataFileName = x$DataFileMetaData$dataFileName,
      columnName = x$DataFileMetaData$columnName,
      IsOK = as.logical(result$details),
      stringsAsFactors = FALSE
    )
  ##
  result$error <- ifelse(
    all(result$details$IsOK),
    0,
    3
  )
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

validateDataFileMetaDataDataFileColumnDefined <- function(x, path) {
  result <- new_emeScheme_validation()
  result$header <- "column names in dataFileName in `columnName`"
  result$description <- paste(
    "Test if column names in `dataFileName` are found in `columnName`.",
    "Only comma separated data files (extension `csv`) will be checked!",
    "The `error` can have the following values apart from `OK`:\n",
    "\n",
    "   error   : If not all column names in `dataFileName` are found in `columnName`\n",
    "\n"
  )
  result$descriptionDetails <- paste(
    "The details are a list of tables, one per `dataFileName`, ",
    "indicating if the column names in `dataFileName` are found in `columnName`.\n",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If column name in `dataFileName` is found in `columnName`\n",
    "   FALSE: If column name in `dataFileName` is not found in `columnName`\n",
    "\n",
    "One or more FALSE will result in an ERROR."
  )
  ##
  dfcol <- readColumnNamesFromDataFiles(x, path)
  result$details <- lapply(
    1:length(dfcol),
    function(i){
      cn <- dplyr::filter(x$DataFileMetaData, .data$dataFileName == names(dfcol[i])) %>%
        dplyr::select(.data$columnName) %>%
        magrittr::extract2(1)
      cn_exp <- as.vector(
        sapply(
          cn,
          function (x) {
            if (grepl("!!!", x)) {
              x <- gsub("!!!", "", x)
              x <- glob2rx(x)
            }
            return(x)
          }
        )
      )

      x <- data.frame(
        dataFile = names(dfcol)[i],
        columnNameInDataFileName = dfcol[[i]],
        IsOK = sapply(
          dfcol[[i]],
          function(x) {
            # cn %in% dfcol[[i]]
            sapply(
              cn_exp,
              function(n) {
                grepl(
                  n,
                  x
                )
              }
            ) %>% any()
          }
        ) %>% as.vector(),
        stringsAsFactors = FALSE
      )
      if (is.na(x[["columnNameInDataFileName"]]) %>% any()) {
        x[["IsOK"]][is.na(x[["columnNameInDataFileName"]])] <- TRUE
      }
      return(x)
    }
  )
  names(result$details) <- names(dfcol)
  #
  result$error <- ifelse(
    all(
      unlist(
        lapply(result$details, "[[", "IsOK")
      )
    ),
    0,
    3
  )
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}

# Internal validation functions -------------------------------------------

validateStructure <- function( x ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Structural / Formal validity"
  result$description <- paste(
    "Test if the structure of the metadata is correct. ",
    "This includes column names, required info, ... ",
    "Should normally be OK, if no modification has been done."
  )
  result$descriptionDetails <- ""
  ##
  struct <- as_dmdScheme( x, keepData = FALSE, verbose = FALSE)
  attr(struct, "propertyName") <- "emeScheme"
  result$details <- all.equal(struct, emeScheme)
  if (isTRUE(result$details)){
    result$error <- 0
  } else {
    result$error <- 3
  }
  ##
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


validateExperiment <- function( x, xraw, xconv ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Experiment"
  result$description <- paste(
    "Test if the metadata concerning **Experiment** is correct. ",
    "This includes column names, required info, ... "
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique validation.\n",
    "The column `Module` contains the name of the validation,\n",
    "The column `error` contains the actual error of the validation.\n",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the validation was `OK`.\n",
    "   FALSE: If the validation was an `error`, `warning` or `note`.\n",
    "   NA   : If at least one v alidation resulted in `NA\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$types <- validateTypes(xraw$Experiment, xconv$Experiment)
  result$suggestedValues <- validateSuggestedValues(xraw$Experiment)
  ##
  result$details <- valErr_isOK(result)
  result$error <- max(valErr_extract(result), na.rm = FALSE)
  if (is.na(result$error)) {
    result$error <- 3
  }
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


validateSpecies <- function( x, xraw, xconv ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Species"
  result$description <- paste(
    "Test if the metadata concerning **Species** is correct. ",
    "This includes column names, required info, ... "
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique validation.\n",
    "The column `Module` contains the name of the validation,\n",
    "The column `error` contains the actual error of the validation.\n",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the validation was `OK`.\n",
    "   FALSE: If the validation was an `error`, `warning` or `note`.\n",
    "   NA   : If at least one v alidation resulted in `NA\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$types <- validateTypes(xraw$Species, xconv$Species)
  result$suggestedValues <- validateSuggestedValues(xraw$Species)
  result$speciesIDUnique <- valitdateSpeciesIDUnique(xraw)
  result$speciesNames <- validateSpeciesNames(xraw)
  ##
  result$details <- valErr_isOK(result)
  result$error <- max(valErr_extract(result), na.rm = FALSE)
  if (is.na(result$error)) {
    result$error <- 3
  }
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


validateTreatment <- function( x, xraw, xconv ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Treatment"
  result$description <- paste(
    "Test if the metadata concerning **Treatment** is correct. ",
    "This includes column names, required info, ... "
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique validation.\n",
    "The column `Module` contains the name of the validation,\n",
    "The column `error` contains the actual error of the validation.\n",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the validation was `OK`.\n",
    "   FALSE: If the validation was an `error`, `warning` or `note`.\n",
    "   NA   : If at least one v alidation resulted in `NA\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$types <- validateTypes(xraw$Treatment, xconv$Treatment)
  result$suggestedValues <- validateSuggestedValues(xraw$Treatment)
  result$parameterInMappinColumn <- validateTreatmentMapping(xraw)
  ##
  result$details <- valErr_isOK(result)
  result$error <- max(valErr_extract(result), na.rm = FALSE)
  if (is.na(result$error)) {
    result$error <- 3
  }
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


validateMeasurement <- function( x, xraw, xconv ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Measurement"
  result$description <- paste(
    "Test if the metadata concerning **Measurement** is correct. ",
    "This includes column names, required info, ... "
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique validation.\n",
    "The column `Module` contains the name of the validation,\n",
    "The column `error` contains the actual error of the validation.\n",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the validation was `OK`.\n",
    "   FALSE: If the validation was an `error`, `warning` or `note`.\n",
    "   NA   : If at least one v alidation resulted in `NA\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$types <- validateTypes(xraw$Measurement, xconv$Measurement)
  result$suggestedValues <- validateSuggestedValues(xraw$Measurement)
  result$measurementIDUnique <- validateMeasurementIDsUnique(xraw)
  result$measuredFrom <- validateMeasurementMeasuredFrom(xraw)
  result$variableInMappinColumn <- validateMeasurementMapping(xraw)
  result$dataExtractionIDInDataExtractionID <- validateMeasurementDataExtraction(xraw)
  ##
  result$details <- valErr_isOK(result)
  result$error <- max(valErr_extract(result), na.rm = FALSE)
  if (is.na(result$error)) {
    result$error <- 3
  }
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


validateDataExtraction <- function( x, xraw, xconv ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "DataExtraction"
  result$description <- paste(
    "Test if the metadata concerning **DataExtraction** is correct. ",
    "This includes column names, required info, ... "
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique validation.\n",
    "The column `Module` contains the name of the validation,\n",
    "The column `error` contains the actual error of the validation.\n",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the validation was `OK`.\n",
    "   FALSE: If the validation was an `error`, `warning` or `note`.\n",
    "   NA   : If at least one v alidation resulted in `NA\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$types <- validateTypes(xraw$DataExtraction, xconv$DataExtraction)
  result$suggestedValues <- validateSuggestedValues(xraw$DataExtraction)
  result$nameInDataExtractionName <- validateDataExtractionIDLinked(xraw)
  ##
  result$details <- valErr_isOK(result)
  result$error <- max(valErr_extract(result), na.rm = FALSE)
  if (is.na(result$error)) {
    result$error <- 3
  }
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


validateDataFileMetaData <- function( x, xraw, xconv, path ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "DataFileMetaData"
  result$description <- paste(
    "Test if the metadata concerning **DataExtraction** is correct. ",
    "This includes column names, required info, ... "
  )
  result$descriptionDetails <- paste(
    "The details are a table with one row per unique validation.\n",
    "The column `Module` contains the name of the validation,\n",
    "The column `error` contains the actual error of the validation.\n",
    "The following values are possible for the column `isTRUE`:\n",
    "\n",
    "   TRUE : If the validation was `OK`.\n",
    "   FALSE: If the validation was an `error`, `warning` or `note`.\n",
    "   NA   : If at least one v alidation resulted in `NA\n",
    "\n",
    "One or more FALSE or missing values values will result in an ERROR."
  )
  ##
  result$types <- validateTypes(xraw$DataFileMetaData, xconv$DataFileMetaData)
  result$allowedValues <- validateAllowedValues(xraw$DataFileMetaData)
  result$dataFilesExist <- validateDataFileMetaDataDataFileExists(xraw, path)
  result$datetimeFormatSpecified <- validateDataFileMetaDataDateTimeSpecified(xraw)
  result$mappingColumnInNameOrParameter <- validateDataFileMetaDataMapping(xraw)
  result$columnNameInDataFileColumn <- validateDataFileMetaDataColumnNameInDataFile(xraw, path)
  result$dataFileColumnInColumnNamen <- validateDataFileMetaDataDataFileColumnDefined(xraw, path)
  ##
  result$details <- valErr_isOK(result)
  result$error <- max(valErr_extract(result), na.rm = FALSE)
  if (is.na(result$error)) {
    result$error <- 3
  }
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


validateDataFiles <- function( x, xraw, xconv, path ){
  result <- new_emeScheme_validation()
  ##
  result$header <- "Data Files"
  result$description <- paste(
    "Test if the data files as mentioned in `DataFileMetaData$dataFileName` is correct. ",
    "This includes column names, required info, ... "
  )
  result$descriptionDetails <- "No further details available."
  ##
  result$error <- NA # max(valErr_extract(result), na.rm = FALSE)
  result$header <- valErr_TextErrCol(result)
  ##
  return(result)
}


