mock_houses <- tribble(
  ~huis_id, ~beleidsvariant, ~simulation_year,
  1, 1, 1,
  1, 1, 2,
  2, 1, 1,
  2, 1, 2
)

mock_sale_func <- function(houses) return(c(0, 1, 0, 2))
mock_rent_func <- function(houses) return(c(0, 2, 0, 1))
mock_rent_tax_func <- function(houses) return(c(0, 0, 0, 1))
mock_sale_tax_func <- function(houses) return(c(0, 0, 0, 0))

mock_funcs <- list(
  sale_func = list(mock_sale_func),
  keep_rent_func = list(mock_rent_func),
  sale_tax_func = list(mock_sale_tax_func),
  keep_rent_tax_func = list(mock_rent_tax_func)
)

mock_parameters <- list(
  discount_rate = 0.05
)

test_that("valid output for valid input", {
  res <-
    pick_sale_or_rent(
      houses = mock_houses,
      parameters = mock_parameters,
      funcs = mock_funcs
    )
  expect_equal(res$rent_or_sell, c('keep_for_rent', 'keep_for_rent', 'sell', 'sell'))

  res <- get_sale_or_rent_cashflow(res)
  expect_equal(res$end_transaction, c(0, 2, 0, 2))
})
