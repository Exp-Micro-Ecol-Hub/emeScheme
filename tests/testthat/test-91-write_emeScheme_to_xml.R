context("91-as_xml_list.emeSchemeSet()")



# Extracts and saves without errors ---------------------------------------

test_that(
  "as_xml_list.emeSchemeSet()",
  {
    expect_known_value(
      object = as_xml_list.emeSchemeSet(dmdScheme_example()),
      file = "ref-91-as_xml_list.emeSchemeSet.method.rda"
    )
  }
)

test_that(
  "as_xml_list.emeSchemeSet()",
  {
    expect_known_value(
      object = as_xml_list(dmdScheme_example()),
      file = "ref-91-as_xml_list.emeSchemeSet.function.rda"
    )
  }
)

