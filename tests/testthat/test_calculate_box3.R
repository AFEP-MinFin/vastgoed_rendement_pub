mock_houses <- tibble(
  init_rent = 12000,
  value_estimate_based_on_woz = 200000,
  woz_indexed = 200000,
  purchase_price = 200000,
  purchase_price_unindexed = 150000,
  simulation_year=2022,
  overdrachtsbelasting = 0.104,
  debt_share = 0.5,
  vermogen = 50000,
  cumcpi = 1,
  box3.percentage = 0.32,
  lwr = 'nieuw',
  belasting_box3_bezit = 0.0617,
  belasting_box3_vrijstelling = 57000,
  belasting_box3_schuld = 0.0257,
  forfait_schijf2_oud = 0.04,
  box3_oud = 'nee',
  investor = 'part_box3'
)

mock_rent_func <- function(x) 12000

test_that("Valid result for valid input", {
  res <- round(calculate_box3(mock_houses, rent_func = mock_rent_func)$box3_tax)
  expect_length(res, 1)
  expect_equal(res, -1982) # 25/07 DvdS paper calculation on 2300, approx rounding errors
})
