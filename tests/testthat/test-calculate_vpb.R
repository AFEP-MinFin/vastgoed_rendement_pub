mock_returns_func <- function(x) {
  list(cashflows = data.frame(
    huis_id = c(1,1),
    aankoop = c(-100, 0),
    end_transaction = c(0, 110),
    a = c(-10, 10),
    b = c(5, 5),
    total_cashflow = c(NA, NA)
  ))
}

mock_houses_sell <- data.frame(
  simulation_year = c(1, 2),
  huis_id = c(1,1),
  aankoop = c(-100, 0),
  sale_cashflow = c(0, 110),
  rent_cashflow = c(0, 110),
  rent_or_sell = 'sell'
)

mock_houses_rent <- data.frame(
  simulation_year = c(1, 2),
  huis_id = c(1,1),
  aankoop = c(-100, 0),
  sale_cashflow = c(0, 110),
  rent_cashflow = c(0, 110),
  rent_or_sell = 'keep_for_rent'
)

test_that("Valid result for valid input", {
  res <- calculate_vpb(mock_houses_sell, mock_returns_func, vpb_rate = 0.1)
  expect_equal(res$vpb, c(0, -2))

  res <- calculate_vpb(mock_houses_rent, mock_returns_func, vpb_rate = 0.1)
  expect_equal(res$vpb, c(0, -1))
})

test_that("Valid result for valid input", {
  res <- .calculate_taxable_profit_carryover(
    c(-10, 20, 30, -5, 15)
  )
  expect_equal(res, c(0, 10, 30, 0, 10))

  res2 <- .calculate_taxable_profit_carryover(
    c(-10, -20, 40, 10)
  )
  expect_equal(res2, c(0, 0, 10, 10))
})

