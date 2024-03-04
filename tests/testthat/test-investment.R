mock_house =
  tibble(
    purchase_price = 100000,
    init_rent = 1000
  )

mock_rent_func <- function(x) tibble(mock_rent = 10000)

testthat::test_that(
  "check valid result for valid input",
  {
    res <- investment_constant_upkeep(mock_house, 0.01)
    expect_length(res$upkeep_payments, 1)
    expect_equal(res$upkeep_payments, -1000)
  }
)

testthat::test_that(
  "check valid result for valid input",
  {
    res <- investment_exploitation_cost(mock_house, mock_rent_func, 0.15)
    expect_length(res, 1)
    expect_equal(res$upkeep_payments, -10000*0.15)
  }
)


mock_house_hmw =
  tibble(
    type = c('EGW', 'MGW'),
    oppervlak = c(47, 120),
    construction_year = c(2004, 2020),
    woz_indexed = c(100000, 200000),
    cumcpi = c(1, 1)
  )

testthat::test_that(
  "check valid result for valid input",
  {
    res <- investment_upkeep_hmw(mock_house_hmw, list(), list())$fixed_cost_hmw
    expect_length(res, 2)
    expect_equal(res[1], -1 * (1692 + 508.9 + 0.011 * 100000)) # 3300.9
    expect_equal(res[2], -1 * (2315 + 499.4 + 0.011 * 200000)) # 5014.4
  }
)
