#' Run Standard Scenario
#'
#' A scenario consists of multiple cashflow streams. The 'standard' scenario
#' includes rent, sale/further rental at the end of the investment period,
#' upkeep costs and taxes. This function makes it easy to run the standard
#' scenario.
#'
#' @param houses Houses df
#' @param upkeep_perc Upkeep cost as percentage of property value
#' @param rent_increase Yearly rent increase (i.e. 0.02)
#' @param debt_share The share of the property value financed with debt
#' @param interest_rate Constant interest rate.
#' @param discount_rate Discount rate (i.e. 0.05)
#' @param exploitation_cost % of rent for upkeep etc
#' @param incidental_investment_year Modeled year of investment
#' @param incidental_cost Cost at incidental_investment_year
#' @param refinance_period After how many years the investor refinances the property
#'
#' @return A list object with multiple types of returns
#' @import dplyr purrr
#' @export

run_box3_scenario <-
  function(houses, parameters) {

    houses <- .data_transformations(houses, parameters)

    rent_income <- partial(
      calculate_rent_all_houses_memoise,
      ... = ,
      growth_newcomer = .086,
      mutation_grade = .15,
      parameters = parameters
    )

  if(is.na(parameters$rent_cap_perc) == F) {
    rent_income_old <- rent_income
    rent_income <- partial(
      cap_rent_at_woz_perc,
      ... = ,
      rent_func = rent_income_old,
      rent_categories = parameters$cap_rent_with_woz_cats,
      cap_perc = parameters$rent_cap_perc
    )
  }

    interest_with_periodic_refinance_partial <-
      partial(
        interest_with_periodic_refinance_mem,
        ... = ,
        debt_share = parameters$debt_share,
        interest_rate = parameters$interest_rates,
        longterm_interest_rate = parameters$interest_rate,
        refinance_period = parameters$refinance_period
      )

    if(is.na(parameters$rent_cap_direct_return) == F) {

      rent_income_old <- rent_income
        rent_income <- partial(
          cap_rent_direct_return_memoised,
          ... = ,
          house_costs_func = costs_partial,
          rent_func = rent_income_old,
          rent_categories = parameters$cap_rent_with_woz_cats,
          direct_return_cap = parameters$rent_cap_direct_return,
          debt_share = parameters$debt_share
        )
    }

     transfer_tax_cashflow_partial <-
      partial(
        calculate_transfer_tax,
          ... = ,
          transfer_tax = parameters$transfer_tax
      )

      costs_partial <- partial(
        calculate_returns,
        ... = ,
        discount_rate = parameters$discount_rate,
        interest_with_periodic_refinance_partial,
        investment_exploitation_cost_partial,
        partial(calculate_box3,
                box3_percentage_2025onwards = parameters$box3_percentage_2025onwards,
                debt_share = parameters$debt_share,
                box3_oud = parameters$box3_oud,
                rent_func = rent_income
        )
      )


    investment_exploitation_cost_partial <-
      partial(
        investment_exploitation_cost,
        ... = ,
        exploitation_cost = parameters$exploitation_cost,
        rent_func = rent_income,
      )

    ### Ready to get rent value in perpetuity?
    final_year_cashflows_partial <- partial(
      calculate_returns,
      ... = ,
      discount_rate = parameters$discount_rate,
      rent_income,
      interest_with_periodic_refinance_partial,
      investment_exploitation_cost_partial,
      partial(calculate_box3_mem,
              box3_percentage_2025onwards = parameters$box3_percentage_2025onwards,
              debt_share = parameters$debt_share,
              box3_oud = parameters$box3_oud,
              rent_func = rent_income
      )
    )

    transaction_keep_property_for_rent_partial <- purrr::partial(
      .f = transaction_keep_property_for_rent,
      ... = ,
      rent_cashflow = final_year_cashflows_partial,
      rent_increase = parameters$rent_increase,
      discount_rate = parameters$discount_rate
    )

    pick_highest_cashflow_partial <- purrr::partial(
      .f = pick_highest_cashflow_mem,
      ... = ,
      discount_rate = parameters$discount_rate,
      transaction_keep_property_for_rent_partial,
      partial(transaction_sell_value_constant_increase, parameters = parameters)
    )

    interest_constant_interest_rate_partial <-
      partial(
        interest_constant_interest_rate,
        ... = ,
        debt_share = parameters$debt_share,
        interest_rate = parameters$interest_rate
      )

    investment_incidental_cost_partial <-
      partial(
        investment_incidental_cost,
        ... = ,
        incidental_cost = parameters$incidental_cost,
        investment_year = parameters$incidental_investment_year
      )

    calculate_box3_partial <- partial(
      calculate_box3_mem,
      box3_percentage_2025onwards = parameters$box3_percentage_2025onwards,
      debt_share = parameters$debt_share,
      transfer_tax = parameters$transfer_tax,
      box3_oud = parameters$box3_oud,
      rent_func = rent_income
    )

    results <- calculate_returns(
      houses = houses,
      discount_rate = parameters$discount_rate,
      rent_income,
      partial(transaction_buy_at_market_value, parameters = parameters),
      interest_with_periodic_refinance_partial,
      transfer_tax_cashflow_partial,
      pick_highest_cashflow_partial,
      investment_exploitation_cost_partial,
      investment_incidental_cost_partial,
      calculate_box3_partial
    )

    print(median(results$stats$irr, na.rm = T))

    return(
        list(
          houses = houses,
          cashflows = results$cashflows,
          stats = results$stats
        )
      )
  }

# For the sake of backwards compatability
#' @export
run_standard_scenario <- run_box3_scenario



#' Run Standard Scenario
#'
#' A scenario consists of multiple cashflow streams. The 'standard' scenario
#' includes rent, sale/further rental at the end of the investment period,
#' upkeep costs and taxes. This function makes it easy to run the standard
#' scenario.
#'
#' @param houses Houses df
#' @param upkeep_perc Upkeep cost as percentage of property value
#' @param rent_increase Yearly rent increase (i.e. 0.02)
#' @param debt_share The share of the property value financed with debt
#' @param interest_rate Constant interest rate.
#' @param discount_rate Discount rate (i.e. 0.05)
#' @param exploitation_cost % of rent for upkeep etc
#' @param incidental_investment_year Modeled year of investment
#' @param incidental_cost Cost at incidental_investment_year
#' @param refinance_period After how many years the investor refinances the property
#'
#' @return A list object with multiple types of returns
#' @import dplyr purrr
#' @export

run_box3_scenario <-
  function(houses, parameters) {

    houses <- .data_transformations(houses, parameters)

    rent_income <- partial(
      calculate_rent_all_houses_memoise,
      ... = ,
      growth_newcomer = .086,
      mutation_grade = .15,
      parameters = parameters
    )

  if(is.na(parameters$rent_cap_perc) == F) {
    rent_income_old <- rent_income
    rent_income <- partial(
      cap_rent_at_woz_perc,
      ... = ,
      rent_func = rent_income_old,
      rent_categories = parameters$cap_rent_with_woz_cats,
      cap_perc = parameters$rent_cap_perc
    )
  }

    interest_with_periodic_refinance_partial <-
      partial(
        interest_with_periodic_refinance_mem,
        ... = ,
        debt_share = parameters$debt_share,
        interest_rate = parameters$interest_rates,
        longterm_interest_rate = parameters$interest_rate,
        refinance_period = parameters$refinance_period
      )

    if(is.na(parameters$rent_cap_direct_return) == F) {

      rent_income_old <- rent_income
        rent_income <- partial(
          cap_rent_direct_return_memoised,
          ... = ,
          house_costs_func = costs_partial,
          rent_func = rent_income_old,
          rent_categories = parameters$cap_rent_with_woz_cats,
          direct_return_cap = parameters$rent_cap_direct_return,
          debt_share = parameters$debt_share
        )
    }

     transfer_tax_cashflow_partial <-
      partial(
        calculate_transfer_tax,
          ... = ,
          transfer_tax = parameters$transfer_tax
      )

      costs_partial <- partial(
        calculate_returns,
        ... = ,
        discount_rate = parameters$discount_rate,
        interest_with_periodic_refinance_partial,
        investment_exploitation_cost_partial,
        partial(calculate_box3,
                box3_percentage_2025onwards = parameters$box3_percentage_2025onwards,
                debt_share = parameters$debt_share,
                box3_oud = parameters$box3_oud,
                rent_func = rent_income
        )
      )


    investment_exploitation_cost_partial <-
      partial(
        investment_exploitation_cost,
        ... = ,
        exploitation_cost = parameters$exploitation_cost,
        rent_func = rent_income,
      )

    ### Ready to get rent value in perpetuity?
    final_year_cashflows_partial <- partial(
      calculate_returns,
      ... = ,
      discount_rate = parameters$discount_rate,
      rent_income,
      interest_with_periodic_refinance_partial,
      investment_exploitation_cost_partial,
      partial(calculate_box3_mem,
              box3_percentage_2025onwards = parameters$box3_percentage_2025onwards,
              debt_share = parameters$debt_share,
              box3_oud = parameters$box3_oud,
              rent_func = rent_income
      )
    )

    transaction_keep_property_for_rent_partial <- purrr::partial(
      .f = transaction_keep_property_for_rent,
      ... = ,
      rent_cashflow = final_year_cashflows_partial,
      rent_increase = parameters$rent_increase,
      discount_rate = parameters$discount_rate
    )

    pick_highest_cashflow_partial <- purrr::partial(
      .f = pick_highest_cashflow_mem,
      ... = ,
      discount_rate = parameters$discount_rate,
      transaction_keep_property_for_rent_partial,
      partial(transaction_sell_value_constant_increase, parameters = parameters)
    )

    interest_constant_interest_rate_partial <-
      partial(
        interest_constant_interest_rate,
        ... = ,
        debt_share = parameters$debt_share,
        interest_rate = parameters$interest_rate
      )

    investment_incidental_cost_partial <-
      partial(
        investment_incidental_cost,
        ... = ,
        incidental_cost = parameters$incidental_cost,
        investment_year = parameters$incidental_investment_year
      )

    calculate_box3_partial <- partial(
      calculate_box3_mem,
      box3_percentage_2025onwards = parameters$box3_percentage_2025onwards,
      debt_share = parameters$debt_share,
      transfer_tax = parameters$transfer_tax,
      box3_oud = parameters$box3_oud,
      rent_func = rent_income
    )

    results <- calculate_returns(
      houses = houses,
      discount_rate = parameters$discount_rate,
      rent_income,
      partial(transaction_buy_at_market_value, parameters = parameters),
      interest_with_periodic_refinance_partial,
      transfer_tax_cashflow_partial,
      pick_highest_cashflow_partial,
      investment_exploitation_cost_partial,
      investment_incidental_cost_partial,
      calculate_box3_partial
    )

    print(median(results$stats$irr, na.rm = T))

    return(
        list(
          houses = houses,
          cashflows = results$cashflows,
          stats = results$stats
        )
      )
  }

# For the sake of backwards compatability
#' @export
run_standard_scenario <- run_box3_scenario
