
# release 0.9.4
Major change
Split into two packages, namely:
* dmdScheme: which contains nearly all of the scheme iundependant functionality, i.e. should work with all chemes defined based on dmdScheme, and 
* emeScheme: containing the actual Ecological Microcosm Experiment Scheme and functionality specific to that scheme

# release 0.9.1
**emeScheme version 0.9.5**
* added descriptions of validation results and details to validate() function
* added version info to package and to xml exports
* added warning to enter_new_metadata() if format = TRUE as it sometimes leads to corrupt files. using format = FALSE fixes this, but the file still contains the example data.

* Due to change in serialization format in R 3.6.0, only compatible with R >= 3.5.0 

* suggestedValues validation has been, in case of a fail, be declassified as a Note (previous: Warning).
* Include wildcards and regular expressions in columnName in DataFileMetaData. Wildcards (i.e. * and ?) have to be enclosed by three exclamation marks (`!!!`), e.g. `Species_!!!*!!!` which will match all `Species_1` as well as `Species_Not KnownSoFar`. Regular expressions do not need to be enclosed with any special characters.
* fix error where a minimum of two rows were imported, even if the second one contains only NAs. Introduced an exclusion of all NA rows (commit db53c83).
* Add error level to print of validation results
>>>>>>> dev
* Add xml import to re-import xml data and validate certain features (types) during the import.

## Fix
* invalid xml export
* fixed missing export of ...ID fields


# release 0.9.1

* added descriptions of validation results and details to validate() function
TO BE ADDED FROM master
>>>>>>> dev

# release 0.9
This should finalise the spreadsheet

* spreadsheet only in package, not on google sheets anymore
* diverse changes in spreadsheet
* reworked validation
* reworked xml export

# release 0.8
This is the first **beta** release for public testing and contains numerous changes from the earlier versions.
The scheme itself as well as the functions should be stable now.

# Rename to emeScheme 0.0.1.0

# microcosmScheme 0.0.0.9000

* Continuous work on scheme
* Added a `NEWS.md` file to track changes to the package.


