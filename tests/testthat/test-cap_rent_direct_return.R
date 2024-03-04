mock_interest_rate_data = tribble(
  ~jaar, ~rente_n,
  2018, 0.05,
  2020, 0.05,
  2021, 0.05,
  2022, 0.05,
  2023, 0.05,
  2024, 0.05
)

mock_rent_func <- function(houses) tibble(mock_rent = houses$init_rent)

interest_with_periodic_refinance_partial <-
  partial(
    interest_with_periodic_refinance,
    ... = ,
    debt_share = 0.5,
    longterm_interest_rate = 0.05,
    refinance_period = 5,
    interest_rates = mock_interest_rate_data
  )

investment_exploitation_cost_partial <-
  partial(
    investment_exploitation_cost,
    rent_func = mock_rent_func,
    ... = ,
    rent_func = mock_rent_func,
    exploitation_cost_perc = 0.15
  )

costs_partial <- partial(
  calculate_returns,
  ... = ,
  discount_rate = 0.055,
  interest_with_periodic_refinance_partial,
  investment_exploitation_cost_partial,
  partial(calculate_box3,
          rent_func = mock_rent_func,
          box3_percentage_2025onwards = 0.35,
          debt_share = 0.5,
          box3_oud = 'nee',
          rent_func = mock_rent_func
  )
)

mock_houses <- tibble(
  value_estimate_based_on_woz = c(377000, 150000),
  woz_indexed = c(377000, 150000),
  init_rent = c(1600, 1250)*12,
  huis_id = c('bl', 'jh'),
  simulation_year = c(2022,2022),
  purchase_price_unindexed = c(300000, 140000),
  purchase_price = c(300000, 140000),
  vermogen = 100000,
  cumcpi = c(1, 1),
  investor = 'part_box3',
  box3oud = 'nee',
  investment_year = c(2020, 2018),
  belasting_box3_schuld = 0.0257,
  belasting_box3_bezit = 0.0617,
  belasting_box3_vrijstelling = 57000,
  box3.percentage = 0.35,
  beleidsvariant = 1,
  postcode = 1,
  rent_category = c('free', 'free')
)


test_that("Valid result for valid input", {
  res <- costs_partial(mock_houses)$cashflows
  expect_equal(nrow(res), 2)
  expect_equal(res$interest_payments, c(-9425, -3750)) # Handmatig nagerekend
  expect_equal(res$upkeep_payments, c(-2880, -2250))   # Handmatig nagerekend
  expect_equal(round(res$box3_tax), c(-5212, -1766))   # Handmatig nagerekend

  res <- cap_rent_direct_return(
    mock_houses,
    house_costs_func = costs_partial,
    rent_func = mock_rent_func,
    rent_categories = 'free',
    direct_return_cap = 0.04,
    debt_share = 0.5
  )
  expect_equal(res$rent_income_capped, c(19200, 10766))

})
