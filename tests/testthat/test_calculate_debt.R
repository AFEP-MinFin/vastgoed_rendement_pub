library(dplyr)
library(vastgoed.rendement)

mock_houses <- tibble(
  value_estimate_based_on_woz = 100000
)

test_that("Valid result for valid input", {
  res <- calculate_debt(mock_houses, debt_share= 0.5)$debt
  expect_length(res, 1)
  expect_equal(res, c(0.5*100000)) # To define
})

test_that("Valid result for valid input", {
  res <- calculate_debt(mock_houses, debt_share= 0.5)$debt
    expect_length(res, 1)
  expect_equal(res, c(0.5*100000)) # To define
})
