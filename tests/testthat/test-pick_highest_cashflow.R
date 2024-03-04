mock_houses = tibble(
  huis_id = c(1, 2, 1, 2),
  beleidsvariant = c(1, 2, 1, 2),
  simulation_year = c(2022, 2023, 2022, 2023)
)

mock_cashflow_1 <- function(houses) {
  tibble(fake_rent = c(12000, 13000, 12000, 13000))
}

mock_cashflow_2 <- function(houses) {
  tibble(fake_rent = c(12000, 13000, 12000, 20000)) # Goes up rapidly for second house
}

mock_cashflow_3 <- function(houses) {
  tibble(fake_rent = c(12500, 12500, 12500, 12500))
}

test_that("Test that correct input delivers correct values", {
  res <- pick_highest_cashflow(
    mock_houses,
    0.1,
    mock_cashflow_1,
    mock_cashflow_2,
    mock_cashflow_3,
    name = 'test_col'
  )

  expect_named(res, 'test_col')
  expect_length(res$test_col, 4)
  expect_equal(as.numeric(res$test_col[2]), 12500)
  expect_equal(as.numeric(res$test_col[4]), 20000)
})
