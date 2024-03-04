library(dplyr)
library(vastgoed.rendement)

mock_houses <- tibble(
  points_woz = 80,
  points_exc_woz = 80,
  wws_points = 160
)



test_that("Valid result for valid input", {
  wws_reform  <-  1
  bA1 <- 1
  bA91 <- 11041
  bA91b <- 172
  bA93i <- 0.33
  res <- floor(correct_woz_cap(mock_houses,threshold = 142)$wws_points)

  expect_length(res, 1)
  expect_equal(res, c(119)) # To define
})
