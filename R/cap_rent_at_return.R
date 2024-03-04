#' cap_rent_at_return
#'

#' @param houses the generic houses dataframe
#' @param rent_func A function that only needs houses as an argument to produce a cashflow stream
#' @param rent_categories Rent categories to which the cap should be applied
#' @param cap_perc The height of the rent cap
#'
#' @return Single column with cashflows
#' @export

cap_rent_at_return <- function(houses, rent_func, rent_categories, return_perc) {

  return(tibble(rent_income = yearly_rent_income_after_cap))
}
