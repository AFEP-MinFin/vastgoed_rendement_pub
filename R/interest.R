#' Functions for interest payments
#'
#' @param houses Houses df
#' @param debt_share What percentage of the house market value
#' @param interest_rate Interest rate on mortgage debt (eg 0.05 for a 5% interest rate)
#'
#' @return A vector with as many elements as houses has rows.
#' @import assertthat memoise
#' @export

interest_constant_interest_rate <- function(houses, debt_share, interest_rate) {
  assert_that( all(has_name(houses, 'purchase_price')) , msg = 'Required variable not available' )
  houses <- calculate_debt(houses,debt_share)
  return(tibble(interest_payments = -1 * houses$debt * interest_rate))
}


interest_with_periodic_refinance <- function(houses, debt_share, refinance_period, interest_rates, longterm_interest_rate) {
  assert_that( all(has_name(houses, 'investment_year')) , msg = 'Required variable not available' )
  houses <- calculate_debt_current_value(houses, debt_share) %>%
    mutate(
  refinance_year = (houses$simulation_year - ((houses$simulation_year - houses$investment_year) %% refinance_period))
    ) %>%
    mutate(interest_rate_dynamic =  unlist(
    mapply(function(x) .lookup_interest_rate(interest_rates,x),
                                        refinance_year)
  ))

  houses$interest_rate_dynamic[houses$refinance_year >= 2022] <- longterm_interest_rate
  interest_payments = (-1 * houses$debt * (houses$interest_rate_dynamic)) #mark up for verhuurhypotheek
  return(tibble(interest_payments))
}

.lookup_interest_rate <- function(interest_rates, year){
  return(interest_rates$rente_n[interest_rates$jaar == year])

}

interest_with_periodic_refinance_mem <- memoise::memoise(interest_with_periodic_refinance)

call_interest_with_periodic_refinance_mem <- function(houses, parameters, funcs) {
  interest_with_periodic_refinance_mem(
      houses = houses,
      debt_share = parameters$debt_share,
      interest_rate = parameters$interest_rates,
      longterm_interest_rate = parameters$interest_rate,
      refinance_period = parameters$refinance_period
    )
}
