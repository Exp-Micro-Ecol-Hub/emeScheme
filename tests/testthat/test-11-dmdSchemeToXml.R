context("11-as_xml()")

test_that(
  "as_xml() raises error with wrong input value",
  {
    expect_error(
      object = as_xml(x = emeScheme_raw),
      regexp = "no applicable method for 'as_xml' applied to an object of class"
    )
  }
)

test_that(
  "roundtrip as_xml() output = metadata",
  {
    expect_equal(
      object = as_xml(x = emeScheme_example, output = "metadata") %>% as_dmdScheme(keepData = TRUE),
      expected = emeScheme_example
    )
  }
)

test_that(
  "roundtrip as_xml() output = complete",
  {
    expect_equal(
      object = as_xml(x = emeScheme_example, output = "complete" ) %>% as_dmdScheme(keepData = TRUE, useSchemeInXml = TRUE),
      expected = emeScheme_example
    )
  }
)


