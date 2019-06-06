context("make_example(.packageName = \"emeScheme\")")


test_that(
  "make_example returns list of examples when called without argument",
  {
    expect_known_output(
      object = make_example(schemeName = "emeScheme"),
      file = "ref-02-make_example_.txt",
    )
  }
)

test_that(
  "make_example() raises error when called with non-existing name",
  {
    expect_error(
      object = make_example(name = "DOES_NOT+EXIST", schemeName = "emeScheme"),
      regexp = "Invalid example. 'name' has to be one of the following values:"
    )
  }
)
