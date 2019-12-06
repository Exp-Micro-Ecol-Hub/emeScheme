context("08-validate()")

x <- dmdScheme_raw()

# fails when x of not supported type --------------------------------------

test_that(
  "validate() fails with unsupported type",
  {
    expect_error(
      object = validate( x = 42 ),
      regexp = "no applicable method for 'validate' applied to an object of class"
    )
  }
)

# returns TRUE -------------------------------------------------------------

test_that(
  "validate() dmdScheme_raw object",
  {
    expect_known_value(
      object = validate( x = x, errorIfStructFalse = TRUE),
      file = "ref-08-validate.dmdScheme.CORRECT.rda"
    )
  }
)

test_that(
  "validate() character( file name )",
  {
    expect_known_value(
      object = validate( x = scheme_path_xlsx(), errorIfStructFalse = TRUE),
      file = "ref-08-validate.character.CORRECT.rda"
    )
  }
)

# returns differences when not correct -------------------------------------

names(x)[1] <- "experiment"

test_that(
  "validate() fails",
  {
    expect_known_value(
      object =  validate( x = x, errorIfStructFalse = FALSE),
      file = "ref-08-validate.DIFFERENCES.rda"
    )
  }
)

# Fails when not correct -----------------------------------------------

test_that(
  "validate() fails",
  {
    expect_error(
      object = validate( x = x, errorIfStructFalse = TRUE),
      regexp = ("Structure of the object to be evaluated is wrong. See the info above for details.")
    )
  }
)

# all reports are correctly created -----------------------------------------------

# TODO
