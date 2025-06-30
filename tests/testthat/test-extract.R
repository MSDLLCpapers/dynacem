# Create a quiet functions
qget_dynfields <- purrr::quietly(get_dynfields)
qread_csv <- purrr::quietly(readr::read_csv)

# Run the function and expect no error
test_that("Run function and expect no error", {
expect_no_error(
  qget_dynfields(
    heemodel = oncpsm,
    payoffs = c("cost_daq_new", "cost_total", "qaly"),
    discount = "disc"
    )
  )
})

# Check that the exported CSV file matches the original object
test_that("Exported CSV matches original object", {
  # Expect
  testexpect <- qget_dynfields(
    heemodel = oncpsm,
    payoffs = c("cost_daq_new", "cost_total", "qaly"),
    discount = "disc",
    fname = "dyntest"
    )
  # Actual
  testactual <- qread_csv("dyntest.csv")
  # Test
  expect_equal(testexpect$result, testactual$result)
  # Remove the created file
  file.remove("dyntest.csv")
})

