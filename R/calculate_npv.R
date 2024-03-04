#' Calculate NPV
#'
#' @param cash_flows Vector of cashflows
#' @param discount_rate Discount rate (i.e. 0.05)
#'
#' @return A NPV value
#' @export


calculate_npv <- function(cash_flows, discount_rate) {
  present_values <- cash_flows / (1 + discount_rate)^(0:(length(cash_flows)-1))
  npv <- sum(present_values)
  return(npv)
}
