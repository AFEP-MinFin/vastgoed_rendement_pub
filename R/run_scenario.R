#' Run  Scenario
#'
#' @param houses Houses df
#' @param parameters
#' @param funcs a list with the elements rent_func, cap_func, aq_cost_func, running_cost_funcs, sale_func, keep_rent_func, and tax_func
#'
#' @return A list object with multiple types of returns
#' @import dplyr purrr
#' @export

run_scenario <-
  function(houses, parameters, funcs) {

    houses <- .data_transformations(houses, parameters)

    partialised_funcs <-
      lapply(funcs, function(v) lapply(v, function(f)
        partial(f, ... = , parameters = parameters, funcs = funcs)))

    # Choose sale or keep for rent
    houses <- bind_cols(
      houses,
      pick_sale_or_rent(
        houses = houses,
        parameters = parameters,
        funcs = partialised_funcs
      )
    )

    results <- call_calculate_returns(
      houses = houses,
      discount_rate = parameters$discount_rate,
      cashflow_funcs = c(
        partialised_funcs$aq_cost_func,
        partialised_funcs$running_cost_funcs,
        partialised_funcs$cap_func,
        partialised_funcs$tax_func,
        get_sale_or_rent_cashflow
      )
    )

    return(
      list(
        houses = houses,
        cashflows = results$cashflows,
        stats = results$stats
      )
    )

  }


#' @export
.data_transformations <- function(houses, parameters) {

  if(is.finite(parameters$home_price_decline)) {
    houses <- houses %>%
      mutate(
        woz_value = woz_value * (1 + parameters$home_price_decline),
        woz_indexed = woz_indexed * (1 + parameters$home_price_decline),
        value_estimate_based_on_woz = value_estimate_based_on_woz * (1 + parameters$home_price_decline),
        value_estimate_indexed_based_on_woz = value_estimate_indexed_based_on_woz * (1 + parameters$home_price_decline)
      )
  }

  if(is.finite(parameters$middenhuur_reform)) {

    if(is.null(parameters$wws_parameters)) {
      wws_parameters = list(bA1 = 1, bA91 = 11041, bA91b = 172, bA93i = 0.33, extra.points.egw = 31, extra.points.mgw = 17, upper_threshold = 187)
    } else {
      wws_parameters = parameters$wws_parameters
    }

    if(parameters$middenhuur_reform == T) {
      houses$investor = 'part_box3'
      houses$huur_oud = 'nee'
      wws_parameters$wws_reform = 1
      houses <-
        houses %>%
        calculate_wws_points(
          .,
          wws_reform = wws_parameters$wws_reform,
          bA1 = wws_parameters$bA1,
          bA91 = wws_parameters$bA91,
          bA91b = wws_parameters$bA91b,
          extra.points.egw = wws_parameters$extra.points.egw,
          extra.points.mgw = wws_parameters$extra.points.mgw
          ) %>%
        correct_woz_cap(.,threshold = wws_parameters$upper_threshold, bA93i = wws_parameters$bA93i) %>%
        calculate_wws_rent(.,parameters$lookup.table) %>%
        create_rent_category(.,142,wws_parameters$upper_threshold) %>%
        get_growth_number()
    }

    if(parameters$middenhuur_reform == F) {
      houses$investor = 'part_box3'
      houses$huur_oud = 'nee'
      wws_parameters$wws_reform = 0
      houses <-
        houses %>%
        calculate_wws_points(
          .,
          wws_reform = wws_parameters$wws_reform,
          bA1 = wws_parameters$bA1,
          bA91 = wws_parameters$bA91,
          bA91b = wws_parameters$bA91b,
          extra.points.egw = wws_parameters$extra.points.egw,
          extra.points.mgw = wws_parameters$extra.points.mgw
        ) %>%
        correct_woz_cap(.,threshold = 142, bA93i = wws_parameters$bA93i) %>%
        calculate_wws_rent(.,parameters$lookup.table) %>%
        create_rent_category(.,142,142) %>%
        get_growth_number()
    }
  }

  if(parameters$middenhuur_bump_percentage != 0) {
    houses <-
      houses %>%
      middenhuur_rent_bump(middenhuur_bump_percentage = parameters$middenhuur_bump_percentage)
  }

  if(parameters$middenhuur_bump_points != 0){
    houses <-
      houses %>%
      middenhuur_points_bump(middenhuur_bump_points = parameters$middenhuur_bump_points, lookup.table = parameters$lookup.table)
  }

  return(houses)
}


run_scenario_mem <- memoise::memoise(run_scenario)
