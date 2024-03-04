library(dplyr)
library(readxl)
library(vastgoed.rendement)
library(testthat)
library(here)


mock_houses <- tibble(
  m2  = c(100, 10),
  energylabelval = c(10, 4),
  energylabeldoelval = c(4, 4),
  jaar_energie_label = '2030',
  year = '2030',
  cumcpi = 1
)

test_that("Valid result for valid input", {
  mock_energy_investment_matrix <-
    readRDS(test_path("fixtures", "mock_energy_investment_matrix.RDS"))

  res <-
    calculate_energy_investment(mock_houses, mock_energy_investment_matrix)$total_energy_investment

  expect_length(res, 2)
  expect_equal(res, c(-35300, 0))
})
