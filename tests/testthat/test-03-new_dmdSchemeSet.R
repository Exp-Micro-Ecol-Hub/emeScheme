context("03-as_dmdScheme()")


# as_dmdScheme --- verbose -----------------------------------------------

test_that(
  "as_dmdScheme() verbose",
  {
    expect_equal(
      object = as_dmdScheme( x = emeScheme_raw, verbose = FALSE ),
      expected = emeScheme
    )
  }
)

test_that(
  "as_dmdScheme() verbose",
  {
    expect_equal(
      object = as_dmdScheme( x = emeScheme_raw, keepData = TRUE, verbose = FALSE ),
      expected = emeScheme_example
    )
  }
)
