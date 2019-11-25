context("01-scheme_active()")


# Test Arguments -------------------------------------------------------------

test_that(
  "scheme_active()$version is equal to the version in the dmdScheme",
  {
    expect_equal(
      object = scheme_active()$version,
      expected = attr(dmdScheme, "dmdSchemeVersion")
    )
  }
)

test_that(
  "scheme_active()$name is equal to the name of the dmdScheme",
  {
    expect_equal(
      object = scheme_active()$name,
      expected = attr(dmdScheme, "dmdSchemeName")
    )
  }
)
