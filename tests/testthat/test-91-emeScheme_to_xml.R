context("91-emeScheme_to_xml()")


skip("Needs to be implemented")
# Extracts and saves without errors ---------------------------------------

## generate updated reference files by running
# emeScheme_to_xml( emeScheme_example, c("rds", "xml"), path = here::here("tests", "testthat"))
##

test_that(
  "emeScheme_to_xml( ) saves rds correctly",
  {
    x <- emeScheme_to_xml( emeScheme_example, file = tempfile(), output = "metadata" )
    ref <- gsub(tempdir(), ".", x)
    expect_equal(
      object = lapply(x, readRDS), ## the generated ones
      expected = lapply(ref, readRDS) # the reference ones stored in the tests directory
    )
  }
)

test_that(
  "emeScheme_to_xml( ) saves xml correctly",
  {
    x <- emeScheme_to_xml( emeScheme_example, "xml", file = tempfile() )
    ref <- gsub(tempdir(), ".", x)
    expect_equal(
      object = lapply(x, readLines), ## the generated ones
      expected = lapply(ref, readLines) # the reference ones stored in the tests directory
    )
  }
)
