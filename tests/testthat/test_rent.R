library(dplyr)
library(vastgoed.rendement)

mock_houses <- tibble(
  init_rent = 1000,
  rent_category =  "free",
  growth = 0.1,
  wws_rent = 800,
  huis_id = 1,
  simulation_year = c(2022,2023),
  beleidsvariant = 1,
  dv = c(0,0.1)
)

mock_parameters <- list(wws_dwingend = 'nee')

test_that("Valid result for valid input", {
  res <- round(calculate_rent_all_houses(mock_houses, growth_newcomer = c(0,0.2), mutation_grade = 0, parameters = mock_parameters)$rent_income)
  expect_length(res, 2)
  expect_equal(res, c(12000,12000*1.1)) # To define
})

mock_houses_regulated = mock_houses %>% mutate(rent_category = 'social')

test_that("Valid result for valid input", {
  res <- round(rent_max_wws(mock_houses, funcs = list(), parameters = mock_parameters)$rent_income)
  expect_length(res, 2)
  expect_equal(res, c(13032,14140)) # To define

  res <- round(rent_max_wws(mock_houses_regulated, funcs = list(), parameters = mock_parameters)$rent_income)
  expect_length(res, 2)
  expect_equal(res, c(800,800)) # To define
})
