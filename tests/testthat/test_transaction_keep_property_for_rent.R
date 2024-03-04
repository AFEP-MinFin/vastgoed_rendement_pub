library(vastgoed.rendement)

mock_house = tibble(
  huis_id = 1,
  beleidsvariant = 1,
  simulation_year = c(2022, 2023),
  init_rent = 1000
)

mock_rent_function <- function(houses) {
  list(cashflows = tibble(total_cashflow = 12000))
}

test_that(
  "correct perpetual npv calculated",
  {
    res <- transaction_keep_property_for_rent(
      mock_house,
      mock_rent_function,
      discount_rate = 0.05,
      rent_increase = 0.01
      )

    expect_equal(res$npv_keep_for_rent[1], 0)
    expect_equal(res$npv_keep_for_rent[2], 300000)
  }
)
