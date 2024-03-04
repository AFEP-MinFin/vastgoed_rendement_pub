#' Calculate Verduurzaming. If label is under a certain target label, investor will invest more money to improve energy label. This is a function of m2, a reference table with cost and a target label
#'
#' @param houses Houses df.
#' @param energy_investment_matrix Matrix with costs per m2 for every step from one label to another.
#' @param energylabelgoalval The minimum energy label goal.
#'
#' @return two column with the cost to get to the target energy label. cost per m2 and total cost.
#' @export


calculate_energy_investment <- function(houses, energy_investment_matrix, energylabelgoalval = 4) {
   houses %>%
    mutate(
      energy_investment_cost_m2 = .lookup_energyinvestment_m2_cost(energylabelval,energylabelgoalval,energy_investment_matrix),
      total_energy_investment = m2 * energy_investment_cost_m2
      ) %>%
    .index_energy_investment_cost() %>%
    .select_energy_investment_target_year() %>%
    select(total_energy_investment)
}

#lookup from reference tabel
.lookup_energyinvestment_m2_cost <- function(current, target,energy_investment_matrix) {
  m2_cost <- as.numeric(energy_investment_matrix[current, target + 1][[1]])
  return(m2_cost)
}

#index the cost based on inflation
.index_energy_investment_cost <- function(houses) {
  houses %>%
    mutate(
      total_energy_investment_indexed = total_energy_investment * cumcpi
      )
}

#select only the year for which investor will invest to get higher label
.select_energy_investment_target_year <- function(houses){
  houses %>%
    mutate(total_energy_investment = case_when(
      year == jaar_energie_label ~ -total_energy_investment_indexed,
      T ~ 0)
    )
}
