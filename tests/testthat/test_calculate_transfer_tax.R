mock_houses <- tibble(
  value_estimate_based_on_woz = 200000,
  simulation_year = 2022
)

test_that("Valid result for valid input", {
  res <- calculate_transfer_tax(mock_houses,0.1)$transfer_tax_cashflow
  expect_length(res, 1)
  expect_equal(res, -20000)
})
