#' cap_rent_at_woz_perc
#'

#' @param houses the generic houses dataframe
#' @param rent_func A function that only needs houses as an argument to produce a cashflow stream
#' @param rent_categories Rent categories to which the cap should be applied
#' @param cap_perc The height of the rent cap
#'
#' @return Single column with cashflows
#' @export

cap_rent_at_woz_perc <- function(houses, rent_func, rent_categories, cap_perc, woz_cap_mininum_rent, interest_rate = 0, interest_rate_weight = 0) {
  yearly_rental_income_no_cap <- rent_func(houses)[[1]]
  yearly_rent_income_cap <- woz_cap_mininum_rent + houses$woz_indexed * (cap_perc + interest_rate * interest_rate_weight)
  yearly_rent_income_after_cap <- pmin(yearly_rental_income_no_cap, yearly_rent_income_cap)

  cap_applies <- houses$rent_category %in% rent_categories
  yearly_rent_income_after_cap[!cap_applies] = yearly_rental_income_no_cap[!cap_applies]

  return(tibble(rent_income = yearly_rent_income_after_cap))
}

cap_rent_at_woz_perc_mem <- memoise::memoise(cap_rent_at_woz_perc)

call_cap_rent_at_woz_perc <- function(houses, parameters, funcs) {

  partialised_funcs <-
    lapply(funcs, function(v) lapply(v, function(f)
      partial(f, ... = , parameters = parameters, funcs = funcs)))

  cap_rent_at_woz_perc_mem(
    houses = houses,
    rent_func = partialised_funcs$rent_func[[1]],
    rent_categories = parameters$cap_rent_with_woz_cats,
    cap_perc = parameters$rent_cap_perc,
    woz_cap_mininum_rent = parameters$woz_cap_mininum_rent,
    interest_rate = parameters$interest_rate,
    interest_rate_weight = parameters$interest_rate_weight
  )
}
