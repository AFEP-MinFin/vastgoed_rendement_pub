library(readxl)
library(openxlsx)
library(here)
library(tidyr)

mock_houses_begin_year = tibble(
  huis_id = c(1, 2, 3),
  beleidsvariant = 1,
  investor = 'part_box3',
  postcode = 'irrelevant',
  energy_label_points = c(40,22,30),
  rent_increase_social_box3 = 0.03,
  rent_increase_social_old = 0.03,
  rent_increase_middle = 0.03,
  rent_increase_social_corp = 0.03,
  rent_increase_free_market =  0.03,
  wws_reform_delta = c(0, 0, 0),
  type = c("MGW", "MGW","MGW"),
  investment_year = c(2018, 2020, 2018),
  purchase_price = c(150000, 320000, 400000),
  purchase_price_unindexed = c(140000, 310000, 285000),
  init_rent = c(1250, 1600, 2500),
  woz_value =  c(150000., 377000, 316000),
  woz_indexed =  c(150000., 377000, 316000),
  value_estimate_based_on_woz  = c(150000., 377000, 316000),
  value_estimate_indexed_based_on_woz = c(150000., 377000, 316000),
  rent_category = c('middle', 'middle', 'middle'),
  wws_rent = c(1000 * 12, 13000, 1000 * 12 * 2),
  belasting_box3_bezit = 0.0617,
  belasting_box3_schuld = 0.0257,
  belasting_box3_vrijstelling = 57000,
  growth = c(0.04, 0.04, 0.04)
)

mock_houses <- setup.data.frame_vec(2022, 15, 3, 1) %>% select(-woz.woning) %>%
  left_join(mock_houses_begin_year, by = c('huis_id', 'beleidsvariant')) %>%
  mutate(simulation_year = year)

mock_houses$cumcpi = rep(cumprod(rep(1.02, 16)), 3)
mock_houses$cumhpi = rep(cumprod(c(0.97, rep(1.02, 15))), 3)
mock_houses$box3.percentage = 0.35

mock_parameters <- list(
  discount_rate = 0.058,
  rent_increase = 0.02,
  upkeep_perc = 0.01,
  debt_share = 0.5,
  exploitation_cost = 0.15,
  incidental_investment_year = 2022,
  incidental_cost = 10000,
  refinance_period = 5, # years
  interest_rate = 0.05,
  transfer_tax = 0.104,
  middenhuur_reform = T,
  lwr = "nieuw",
  box3_oud = 'nee',
  middenhuur_bump_percentage = 0,
  middenhuur_bump_points = 0,
  rent_cap_perc = NA,
  rent_func = NA,
  wws_dwingend = 'ja',
  rent_cap_direct_return = NA,
  home_price_decline = -0.06,
  lookup.table = readRDS(test_path("fixtures", "mock_lookup_table.RDS")),
  interest_rates = readRDS(test_path("fixtures", "mock_interest_rates.RDS"))
)

test_that("Valid result for valid input", {
  res <- run_standard_scenario(mock_houses, mock_parameters)
  expect_equal(nrow(res$cashflows), 48)
  expect_equal(round(res$stats$irr, 3), c(0.162, 0.022, 0.156)) # To define
})
