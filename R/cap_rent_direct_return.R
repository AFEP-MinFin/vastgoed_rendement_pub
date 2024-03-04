#' cap_rent_direct_return
#'
#' @param houses the generic houses dataframe
#' @param rent_func A function that only needs houses as an argument to produce a cashflow stream
#' @param rent_categories Rent categories to which the cap should be applied
#' @param cap_perc The height of the rent cap
#' @param house_costs_func A partialised get_returns function
#'
#' @return Single column with cashflows
#' @export
#' @import memoise

cap_rent_direct_return <- function(houses, house_costs_func, rent_func, rent_categories, direct_return_cap, debt_share) {
  yearly_rental_income_no_cap <- rent_func(houses)[[1]]

  cost_cashflows <- house_costs_func(houses)$cashflows

  yearly_rent_income_cap <- houses$woz_indexed * (1 - debt_share) * direct_return_cap + (-1 * cost_cashflows$total_cashflow)

  yearly_rent_income_after_cap <- pmin(yearly_rental_income_no_cap, yearly_rent_income_cap)

  cap_applies <- houses$rent_category %in% rent_categories
  yearly_rent_income_after_cap[!cap_applies] = yearly_rental_income_no_cap[!cap_applies]

  return(tibble(rent_income_capped = round(yearly_rent_income_after_cap)))
}

cap_rent_direct_return_memoised <- memoise::memoise(cap_rent_direct_return)

no_cap <- function(houses, parameters, funcs) {
  funcs$rent_func[[1]](houses = houses, parameters = parameters, funcs = funcs)
}
