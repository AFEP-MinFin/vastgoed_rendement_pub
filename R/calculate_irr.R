#' Calculate IRR
#'
#' @param cash_flows Vector of cash flows
#'
#' @return A IRR value
#' @export
calculate_irr <- function(cash_flows) {
  tolerance <- 0.0001  # Tolerance level for convergence

  npv <- partial(calculate_npv, cash_flows = cash_flows)

  irr <- uniroot(npv, interval = c(0, 1 + max(cash_flows)/abs(min(cash_flows))), tol = tolerance,extendInt = "yes")$root

  return(irr)
}
