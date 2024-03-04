library(dplyr)
library(vastgoed.rendement)

mock_houses <- tibble(
  rent = 12000,
  woz_indexed = 250000
)

test_that("Valid result for valid input", {
  res <- calculate_empty_house_value(mock_houses, lwr= "nieuw")$empty_house_value
  expect_length(res, 1)
  expect_equal(res, c(.95*250000)) # To define
})

test_that("Valid result for valid input", {
  res <- calculate_empty_house_value(mock_houses, lwr= "oud")$empty_house_value
  expect_length(res, 1)
  expect_equal(res, c(.67*250000)) # To define
})
