context("03-as_dmdScheme()")


# as_dmdScheme --- verbose -----------------------------------------------

test_that(
  "as_dmdScheme() verbose",
  {
    expect_equal(
      object = as_dmdScheme( x = dmdScheme_raw(), verbose = FALSE ),
      expected = dmdScheme()
    )
  }
)

test_that(
  "as_dmdScheme() verbose",
  {
    expect_equal(
      object = as_dmdScheme( x = dmdScheme_raw(), keepData = TRUE, verbose = FALSE ),
      expected = dmdScheme_example()
    )
  }
)
