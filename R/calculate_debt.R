#' Calculate Debt of the investment
#'

#' @param houses Houses df
#' @param debt_share Percentage of home value as mortgage debt
#' @param transfer_tax Transfer tax rate
#' @return A column with debt based on inputs purchase price, debt-share and transfer tax
#' @export

calculate_debt <- function(houses, debt_share){
  houses <- houses %>%
    mutate(
      debt = value_estimate_based_on_woz *  debt_share
      )
  return(houses)
}

calculate_debt_current_value <- function(houses, debt_share){
  houses <- houses %>%
    mutate(
      debt = value_estimate_based_on_woz *  debt_share
    )
  return(houses)
}
