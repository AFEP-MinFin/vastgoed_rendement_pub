library(here)
source(here('dependencies.R'))
devtools::load_all()
print(here())

library(GA)


houses <- read_feather(here('nonsensitive_data_files/houses_realstats_2023-11-07.feather')) %>% filter(is.finite(purchase_price))
wb <- loadWorkbook(here('nonsensitive_input_files/modelparams_testhuizen_all2.xlsx'))
realstatshuizen_subset <- read.xlsx(wb, sheet = "voorbeeldhuizen" )
lookup.table <- read.xlsx(wb, sheet = 'wws_lookup')
interest_rates <- read.xlsx(wb, sheet = "Hypothecaire_rente" )

#houses <- houses %>% filter(huis_id %in% rep(1:100))

house_properties <- houses %>%
  group_by(huis_id, beleidsvariant) %>%
  summarise(
    investment_year = first(investment_year),
    wws_points = first(wws_points),
    oppervlak = first(oppervlak),
    woz_value = first(woz_value),
    value_estimate_2022 = first(value_estimate_based_on_woz),
    rent_category = first(rent_category)
  )

interest_rates <- read.xlsx(wb, sheet = "Hypothecaire_rente" )

houses$aankoopwaarde = houses$purchase_price
houses$overdrachtsbelasting = as.numeric(houses$overdrachtsbelasting)
houses$belasting_box3_vrijstelling = as.numeric(houses$belasting_box3_vrijstelling)
houses$box3.percentage = as.numeric(houses$box3.percentage)
houses$belasting_box3_bezit = as.numeric(houses$belasting_box3_bezit)
houses$belasting_box3_schuld = as.numeric(houses$belasting_box3_schuld)



parameters_sq <- list(
  discount_rate = 0.0513,
  rent_increase = 0.02,
  upkeep_perc = 0.01,
  debt_share = 0.5,
  exploitation_cost = 0.15,
  incidental_investment_year = 2022,
  incidental_cost = 10000,
  refinance_period = 5, # years
  interest_rate = 0.052,
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
  interest_rates = interest_rates,
  home_price_decline = -0.0625,
  lookup.table = lookup.table,
  vpb_rate = 0.19,
  box2_rate = 0.245
)

box3_funcs <- list(
  rent_func = list(call_calculate_rent_all_houses),
  cap_func = list(no_cap),
  aq_cost_func = list(transaction_buy_at_market_value),
  running_cost_funcs = list(
    call_interest_with_periodic_refinance_mem,
    call_calculate_transfer_tax,
    call_investment_incidental_cost,
    call_investment_exploitation_cost
  ),
  sale_func = list(call_transaction_sell_value_constant_increase),
  keep_rent_func = list(call_transaction_keep_property_for_rent),
  tax_func = list(call_calculate_box3),
  sale_tax_func = list(call_calculate_box3_transaction_only),
  keep_rent_tax_func = list(call_calculate_box3_transaction_only)
)


optimize_func <- function(params) {
  parameters_opt <- parameters_sq
  parameters_opt$wws_parameters <- list(
    bA1 = params[1],
    bA91 = params[2],
    bA91b = params[3],
    bA93i = params[4]
  )

  res <- run_scenario(houses, parameters_opt, box3_funcs)
  score <- sum((res$stats$irr - 0.0513)^2, na.rm = T)
  print(score)
  return( score )
}


initial_guess <- c(bA1 = 1, bA91 = 10000, bA91b = 172, bA93i = 0.33)
result <- optim(par = initial_guess, fn = optimize_func, method = "BFGS")

print(result)

save(result)
