context("10-read_excel(schemeName = \"dmdScheme\")")


# fail because of file -------------------------------------------------------------

test_that(
  "read_excel() fails when file does not exist",
  {
    expect_error(
      object = read_excel("DOES_NOT_EXIST"),
      regexp = "No such file or directory"
    )
  }
)


test_that(
  "read_excel() fails when file does not have right extension",
  {
    expect_error(
      object = read_excel("DummyForTests"),
      regexp = "If x is a file name, it has to have the extension 'xls' or 'xlsx'"
    )
  }
)


# read from xlsx --- value ----------------------------------------------------------

test_that(
  "read_excel() keepData and raw",
  {
    expect_equal(
      object = read_excel(
        file = scheme_path_xlsx(),
        keepData = TRUE,
        raw = TRUE,
        verbose = FALSE
      ),
      expected = dmdScheme_raw()
    )
  }
)

test_that(
  "read_excel() keepData no data and not raw",
  {
    expect_equal(
      object = read_excel(
        file = scheme_path_xlsx(),
        keepData = FALSE,
        raw = FALSE,
        verbose = FALSE
      ),
      expected = dmdScheme()
    )
  }
)
#

# # read from xlsx --- output -----------------------------------------------
#
# test_that(
#   "read_excel() keepData and raw",
#   {
#     expect_known_output(
#       object = read_excel(
#         file = system.file("emeScheme.xlsx", package = "emeScheme"),
#         keepData = TRUE,
#         raw = TRUE,
#         verbose = TRUE
#       ),
#       file = "emeScheme_data_raw.output"
#     )
#   }
# )

