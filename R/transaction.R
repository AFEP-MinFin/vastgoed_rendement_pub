#' Functions for property transactions
#'
#' @param houses Houses df
#'
#' @return A vector with as many elements as houses has rows.
#' @import assertthat
#' @export

transaction_buy_at_market_value <- function(houses, parameters, funcs) {
  assert_that( all(has_name(houses, 'purchase_price')) , msg = paste0('Required variable not available',' - purchase_price' ))

  start_year <- min(houses$simulation_year)
  buy <- -1 * case_when(houses$simulation_year == start_year ~ houses$value_estimate_based_on_woz * (1-parameters$debt_share), T ~ 0)
  return(tibble(aankoop = buy))
}

#' transaction_sell_at_orignal_value
#'
#' @param houses Houses df
#'
#' @export
transaction_sell_at_orignal_value <- function(houses) {
  assert_that( all(has_name(houses, 'purchase_price')) , msg = paste0('Required variable not available','- purchase_price' ))
  end_year <- max(houses$simulation_year)
  sell <- case_when(houses$simulation_year == end_year ~ houses$value_estimate_based_on_woz, T ~ 0)

  return(tibble(verkoop = sell))
}

#' transaction_sell_value_constant_increase
#'
#' @param houses Houses df
#' @param increase_factor yearly price increase
#'
#' @export
transaction_sell_value_constant_increase <- function(houses, parameters = parameters) {
  assert_that( all(has_name(houses, c('purchase_price', 'huis_id', 'beleidsvariant'))) , msg = 'Required variable not available' )
  end_year <- max(houses$simulation_year)

  sell <- case_when(houses$simulation_year == end_year ~ houses$value_estimate_indexed_based_on_woz - houses$value_estimate_based_on_woz * parameters$debt_share, T ~ 0)

  return(tibble(verkoop = sell))
}

call_transaction_sell_value_constant_increase <- function(houses, parameters, funcs) {
  transaction_sell_value_constant_increase(
    houses = houses,
    parameters = parameters
  )
}

#' Keep property for Rent
#'
#' @param houses Houses df
#'
#' @param rent_cashflow Rental income
#' @param rent_increase Assumed yearly rent increase (0.01 = 1% assumed rent increase)
#' @param discount_rate Investor discount rate for future returns (such as 0.058)
#'
#' @export
transaction_keep_property_for_rent <- function(houses, rent_cashflow, rent_increase = 0.02, discount_rate = 0.0513) {

  houses$total_cashflow <- rent_cashflow(houses)$cashflows$total_cashflow
  end_year <- max(houses$simulation_year)

  sell <- houses %>%
    mutate(
      npv_keep_for_rent = case_when(
        simulation_year == end_year ~ (total_cashflow) / (discount_rate - rent_increase),
        T ~ 0
      )
    ) %>%
    select(npv_keep_for_rent)

  return(sell)

}

transaction_keep_property_for_rent_mem <- memoise::memoise(transaction_keep_property_for_rent)

call_transaction_keep_property_for_rent <- function(houses, parameters, funcs) {

  partialised_funcs <-
    lapply(funcs, function(v) lapply(v, function(f)
      partial(f, ... = , parameters = parameters, funcs = funcs)))

  partial_cashflows <- partial(call_get_cashflows,
    funs = c(
      partialised_funcs$running_cost_funcs,
      partialised_funcs$cap_func,
      partialised_funcs$tax_func
    )
  )

  transaction_keep_property_for_rent_mem(
    houses = houses,
    rent_cashflow = partial_cashflows,
    rent_increase = 0.02,
    discount_rate = parameters$discount_rate
  )
}
