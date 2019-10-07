#' Convert file name or \code{emeScheme} object to one xml per data file
#'
#' Converts an \code{emeScheme} object into xml and optionally saves it to a file.
#'
#' Depending on the value of \code{file}, the function returns different results:
#' \itemize{
#'   \item{\code{file} not given: }{an \code{XMLNode} object}
#'   \item{\code{file = NULL}: }{a string characterisation of the \code{XMLNode} object}
#'   \item{\code{file = fileName}: }{saves the string characterisation of the \code{XMLNode} object to an xml file}
#' }
#' The actual implementation is inspired by David LeBauer, Carl Davidson, Rob Kooper. See \url{https://stackoverflow.com/a/27865512/632423}
#' @param x \code{emeScheme} object to be converted.
#'
#' @param file empty, \code{NULL} or file name. See details below
#' @param output either \code{"metadata"} for export of metadata only or
#'   \code{"complete"} for export including classes et al.
#'
#' @return dependent on the value of \code{file}. See Details
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' emeScheme_to_xml(emeScheme_example)
#' ## x is a list containing all the emeSchemes for each data file
#'
#' emeScheme_to_xml(emeScheme_example, file = tempdir())
#' ## saves the resulting object as xml into the tmpdir()

emeScheme_to_xml <- function(
  x,
  file,
  output = "metadata"
) {

# Check arguments ---------------------------------------------------------

  if (!methods::is(x, "emeSchemeSet")) {
    x <- read_excel(x)
  }

  if (!methods::is(x, "emeSchemeSet")) {
    stop("x has to be an object of type emeSchemeSet or file name of metadata spreadsheet.")
  }

  if (!missing(file)) {
    fns <- NULL
    path <- dirname(file)
    fn <- basename(file)
  } else {
    file <- NULL
  }

# Extract DataFileMetaData$dataFileNames ----------------------------------

  dataFileName <- unique(x$DataFileMetaData$dataFileName)
  names(dataFileName) <- dataFileName

# Extract for each unique dataFileName ------------------------------------

  splitted <- lapply(
    dataFileName,
    emeScheme_extract,
    x = x
  )

# Save if asked for -------------------------------------------------------

  result <- lapply(
    splitted,
    function(x) {
      if (is.null(file)) {
        dmdScheme::dmdScheme_to_xml(x, output = output)
      } else {
        fn <- file.path( path, paste(fn, attr(x, "propertyName"), "xml", sep = ".") )
        dmdScheme::dmdScheme_to_xml(x, file = fn, output = output)
        fns <<- c(fns, fn)
      }
    }
  )
  if (!is.null(file)) {
    result <- fns
  }

# Return ------------------------------------------------------------------

  return(result)
}


