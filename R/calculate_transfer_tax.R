#' Calculate Debt of the investment
#'

#' @param houses Houses df
#' @param transfer_tax Transfer tax rate
#' @return A column with a negative transfer tax cashflow, only in the first year
#' @export

calculate_transfer_tax <- function(houses, transfer_tax){
  assert_that( all(has_name(houses, c('value_estimate_based_on_woz','simulation_year'))) , msg = 'Required variable not available' )
  transfer_tax_cashflow = case_when(houses$simulation_year == 2022 ~ -1 * houses$woz_indexed * transfer_tax,
                                    T ~ 0)
  return(tibble(transfer_tax_cashflow))
}

call_calculate_transfer_tax <- function(houses, parameters, funcs) {
  calculate_transfer_tax(houses = houses, transfer_tax = parameters$transfer_tax)
}
