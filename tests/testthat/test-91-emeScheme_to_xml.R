context("emeScheme_to_xml()")


# Test Arguments -------------------------------------------------------------

test_that(
  "emeScheme_to_xml( ) fails when x does not exist",
  {
    expect_error(
      object = emeScheme_to_xml( "NOT_VALID" ),
      regexp = "Can not open file 'NOT_VALID': No such file or directory"
    )
  }
)


test_that(
  "emeScheme_to_xml( ) fails when outpot not valid",
  {
    expect_error(
      object = emeScheme_to_xml( emeScheme_example, output = "NOT_VALID"),
      regexp = "Wrong value for 'output'. 'output' has to be one of the following values:metadata complete"
    )
  }
)

# Estracts and saves without errors ---------------------------------------

## generate updated reference files by running
# emeScheme_to_xml( emeScheme_example, file = here::here("tests", "testthat", "ref-91-emeScheme_to_xml"), output = "metadata")
##


test_that(
  "emeScheme_to_xml( ) saves xml correctly",
  {
    fn <- tempfile()
    x <- emeScheme_to_xml( emeScheme_example, file = fn )
    ref <- gsub(fn, "./ref-91-emeScheme_to_xml", x)
    expect_equal(
      object = lapply(x, readLines), ## the generated ones
      expected = lapply(ref, readLines) # the reference ones stored in the tests directory
    )
  }
)
