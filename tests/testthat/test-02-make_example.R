context("02-make_example()")


test_that(
  "make_example returns list of examples when called without argument, i,.e. here empty",
  {
    expect_message(
      object = make_example( schemeName = "emeScheme"),
      file = "Included examples are:",
    )
  }
)

test_that(
  "make_example() raises error when called with non-existing name",
  {
    expect_error(
      object = make_example(name = "DOES_NOT+EXIST"),
      regexp = "Invalid example. 'name' has to be one of the following values:"
    )
  }
)

