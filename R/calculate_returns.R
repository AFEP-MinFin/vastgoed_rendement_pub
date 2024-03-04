#' Calculate returns
#'
#' @param houses Houses df
#' @param discount_rate Discount rate (i.e. 0.05)
#' @param ... Arbitrary number of functions that take houses as an argument and return a vector of cashflows
#'
#' @return A list object with multiple types of returns
#' @import jrvFinance
#' @export

calculate_returns <- function(houses, discount_rate, ...) {
  funs <- list(...)

  cashflows <- get_cashflows(funs, houses) %>%
    bind_cols(houses %>%
                select(huis_id, beleidsvariant, postcode))

  stats <- cashflows %>%
    group_by(huis_id, beleidsvariant, postcode) %>%
    summarise(irr = tryCatch({
      calculate_irr(total_cashflow)
    }, error = function(e)
      NA),
    npv = calculate_npv(total_cashflow, discount_rate),)

  return(list(cashflows = cashflows,
              stats = stats))
}

call_calculate_returns <- function(
    houses,
    discount_rate,
    cashflow_funcs
    ) {
  do.call(calculate_returns,
          c(
            list(
              houses = houses,
              discount_rate = discount_rate
              ),
            cashflow_funcs
          ))
}

#' @export
get_cashflows <- function(funs, houses) {
  lapply(funs, function(x) x(houses)) %>%
    bind_cols() %>%
    mutate(total_cashflow = rowSums(across(everything())))
}

call_get_cashflows <- function(funs, houses) {
  list(cashflows = get_cashflows(funs, houses))
}
