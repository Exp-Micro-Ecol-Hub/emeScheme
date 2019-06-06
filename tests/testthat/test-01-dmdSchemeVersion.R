context("dmdScheme_versions()")


# Test Arguments -------------------------------------------------------------

test_that(
  "dmdScheme_versions()$scheme is equal to the version in the dmdScheme",
  {
    expect_equal(
      object = dmdScheme_versions(schemeName = "emeScheme")$scheme,
      expected = as.numeric_version(attr(dmdScheme, "dmdSchemeVersion"))
    )
  }
)

test_that(
  "dmdScheme_versions()$package is equal to packageVersion('dmdScheme')",
  {
    expect_equal(
      object = dmdScheme_versions()$package,
      expected = packageVersion("dmdScheme")
    )
  }
)
