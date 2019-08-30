#' Split \code{emeScheme} object into multiple by using \code{DataFileNameMetaData$dataFileName}
#'
#' One \code{emeScheme} object can contain metadata for multiple datafiles. For
#' archiving, these should be split into single \code{emeScheme} objects, one
#' for each \code{DataFileNameMetaData$dataFileName}.
#' \bold{ATTENTION: Files are overwritten without warning!!!!!}
#' @param x file name of spreadsheet containing the metadata of an emeScheme or
#'   \code{emeScheme} object to be split and exported to xml
#' @param saveAsType if given, save the result to files named following the
#'   pattern \code{DATAFILENAME_attr(x, "propertyName").saveAsType}. If missing,
#'   do not save and return the result. Allowed values at the moment:
#'   \itemize{
#'    \item{none} {do not save the resulting list}
#'    \item{rds} {save results in files using \code{saveRDS()}}
#'    \item{xml} {save results in xml files using \code{emeSchemeToXml()}}
#'    \item{multiple values of the above} {will be saved in all specified formats}
#'   }
#' @param path path where the files should be saved to
#'
#' @return if \code{saveAsType} valid and not equal to \code{none}, \code{character} vector
#'   containing the file names where the splitted metadata has been saved to, if
#'   \code{saveAsType} missing, \code{list} where each element is one
#'   \code{emeScheme} object for a data file as specified in
#'   \code{DataFileNameMetaData$dataFileName}.
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' emeScheme_split(emeScheme_example)
#' ## x is a list containing all the emeSchemes for each data file
#'
#' emeScheme_split(emeScheme_example, saveAsType = "rds", path = tempdir())
#' ## saves the resulting object as rds using saveRDS() into the tmpdir()
#'
#' emeScheme_split(emeScheme_example, saveAsType = "xml", path = tempdir())
#' ## saves the resulting object as xml into the tmpdir()
#'
#' emeScheme_split(emeScheme_example, saveAsType = c("rds", "xml"), path = tempdir())
#' ## saves the resulting object as rds and xml into the tmpdir()

emeScheme_to_xml <- function(
  x,
  file,
  path = "."
) {

# Check arguments ---------------------------------------------------------

  if (!methods::is(x, "emeSchemeSet")) {
    x <- read_from_excel(x)
  }

  if (!methods::is(x, "emeSchemeSet")) {
    stop("x has to be an object of type emeSchemeSet")
  }

# Extract DataFileMetaData$dataFileNames ----------------------------------

  dataFileName <- unique(x$DataFileMetaData$dataFileName)
  names(dataFileName) <- dataFileName

# Extract for each unique dataFileName ------------------------------------

  result <- lapply(
    dataFileName,
    emeScheme_extract,
    x = x
  )

# Save if asked for -------------------------------------------------------

  if (!missing(file)) {
    fns <- NULL
    path <- dirname(file)
    fn <- basename(file)
    lapply(
      result,
      function(x) {
        fn <- file.path( path, paste(fn, attr(x, "propertyName"), "xml", sep = ".") )
        dmdScheme::dmdScheme_to_xml(x, file = fn)
        fns <<- c(fns, fn)
      }
    )
    result <- fns
  }

# Return ------------------------------------------------------------------

  return(result)
}


