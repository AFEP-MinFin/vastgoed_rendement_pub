library(dplyr)
library(vastgoed.rendement)

mock_houses <- tibble(
  oppervlak = 100,
  woz.woning = 200000,
  woz_indexed = 200000,
  type = "MGW",
  energy_label_points = 10,
  wws_reform_delta = 8,
  cumhpi=1
)



test_that("Valid result for valid input", {
  extra.points.egw <- 31
  extra.points.mgw <- 17
  wws_reform <- 1
  bA1 <- 1
  bA91 <- 11041
  bA91b <- 172
  bA93i <- 0.33
  res <- floor(calculate_wws_points(mock_houses, wws_reform = 1, bA1 = 1, bA91 = 11041, bA91b = 172)$wws_points)

  expect_length(res, 1)
  expect_equal(res, c(164)) # To define
})
