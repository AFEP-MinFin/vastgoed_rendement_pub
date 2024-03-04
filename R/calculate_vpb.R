#' Calculate Box 2  tax , based,  assets and debt.

#' Source VPB rate: https://www.belastingdienst.nl/wps/wcm/connect/bldcontentnl/belastingdienst/zakelijk/winst/vennootschapsbelasting/tarieven_vennootschapsbelasting
#' Source dividend rate: https://www.rijksoverheid.nl/onderwerpen/inkomstenbelasting/2-belastingschijven-box-2

#' @param houses houses
#' @param returns_func Functions with all relevant cashflows (including sale)
#' @param vpb_rate VPB tax rate
#' @param box2_rate Dividend tax rate
#'
#' @export

calculate_vpb_and_box2_tax <- function(houses, returns_func, vpb_rate = 0.26, box2_rate = 0.245) {
  vpb <- calculate_vpb_mem(houses, returns_func, vpb_rate)
  box2_tax <- ((vpb$vpb / vpb_rate) - vpb$vpb) * box2_rate
  return(tibble(vpb_and_box2 = vpb$vpb + box2_tax))
}


#' @param houses houses
#' @param returns_func Functions with all relevant cashflows (including sale)
#' @param vpb_rate VPB tax rate
#' @param dividend_rate Dividend tax rate
#'
#' @export
calculate_vpb <- function(houses, returns_func, vpb_rate = 0.258) {

  all_cashflows <- returns_func(houses)$cashflows
  all_cashflows$simulation_year = houses$simulation_year
  all_cashflows$huis_id = houses$huis_id
  end_year <- max(houses$simulation_year)
  begin_year <- min(houses$simulation_year)

  if('rent_or_sell' %in% colnames(houses)) {

    print('calculate_vpb belast winst bij verkoop')

    # eerst vaststellen over de verkoop winsten zijn en die belasten in verkoop jaar
    all_cashflows$rent_or_sell <- houses$rent_or_sell
    all_cashflows$sale_cashflow <- houses$sale_cashflow
    all_cashflows$rent_cashflow <- houses$rent_cashflow

    all_cashflows <-
      all_cashflows %>%
        group_by(huis_id) %>%
        mutate(
          taxable_profit = case_when(
            simulation_year == end_year & rent_or_sell == 'sell' ~ first(aankoop) + last(sale_cashflow),
            simulation_year == end_year & rent_or_sell == 'keep_for_rent' ~ 0,
            T ~ 0
          )
        )

    all_cashflows$rent_or_sell <- NULL
    all_cashflows$sale_cashflow <- NULL
    all_cashflows$rent_cashflow <- NULL
  }

  all_cashflows <-
    all_cashflows %>%
    mutate(
      taxable_cashflow_excl_carryover = rowSums(across(
        !matches(c('huis_id', 'simulation_year', 'transfer_tax_cashflow', 'verkoop', 'aankoop', 'npv_keep_for_rent', 'sale_cashflow', 'rent_cashflow', 'end_transaction', 'total_cashflow'))))
    )

  # je wil eerst alle verliezen verrekenen...
  all_cashflows <-
    all_cashflows %>%
    group_by(huis_id) %>%
    mutate(
      taxable_cashflow = .calculate_taxable_profit_carryover_mem(taxable_cashflow_excl_carryover)
    )

  # en dan belasting heffen over cashflows
  return(tibble(vpb = -1 * vpb_rate * all_cashflows$taxable_cashflow))

}

#' @param taxable_cashflow_excl_carryover vector of profits and losses
#'
#' @export
.calculate_taxable_profit_carryover <- function(taxable_cashflow_excl_carryover) {

  # Bereken de cumulatieve som van cashflows
  cumulatieveCashflows <- cumsum(taxable_cashflow_excl_carryover)

  # Bereken de belastbare winst voor elk jaar
  belastbareWinst <- numeric(length(taxable_cashflow_excl_carryover))
  opgeslagenVerlies <- 0

  for (i in 1:length(taxable_cashflow_excl_carryover)) {
    huidigeCashflow <- taxable_cashflow_excl_carryover[i]

    # Verwerk verliezen
    if (huidigeCashflow < 0) {
      opgeslagenVerlies <- opgeslagenVerlies + abs(huidigeCashflow)
      belastbareWinst[i] <- 0
    } else {
      # Verreken winsten met opgeslagen verliezen
      if (opgeslagenVerlies > 0) {
        if (huidigeCashflow > opgeslagenVerlies) {
          belastbareWinst[i] <- huidigeCashflow - opgeslagenVerlies
          opgeslagenVerlies <- 0
        } else {
          opgeslagenVerlies <- opgeslagenVerlies - huidigeCashflow
          belastbareWinst[i] <- 0
        }
      } else {
        belastbareWinst[i] <- huidigeCashflow
      }
    }
  }

  return(belastbareWinst)

}

.calculate_taxable_profit_carryover_mem <- memoise::memoise(.calculate_taxable_profit_carryover)

calculate_vpb_mem <- memoise::memoise(calculate_vpb)

call_calculate_vpb <- function(houses, parameters, funcs) {

  partialised_funcs <-
    lapply(funcs, function(v) lapply(v, function(f)
      partial(f, ... = , parameters = parameters, funcs = funcs)))

  partial_cashflows <- partial(call_get_cashflows,
                               funs = c(
                                 partialised_funcs$aq_cost_func,
                                 partialised_funcs$running_cost_funcs,
                                 partialised_funcs$cap_func
                               )
  )

  calculate_vpb_mem(
    houses,
    returns_func = partial_cashflows,
    vpb_rate = parameters$vpb_rate
  )
}

call_calculate_vpb_and_box2_tax <- function(houses, parameters, funcs) {

  partialised_funcs <-
    lapply(funcs, function(v) lapply(v, function(f)
      partial(f, ... = , parameters = parameters, funcs = funcs)))

  partial_cashflows <- partial(call_get_cashflows,
                               funs = c(
                                 partialised_funcs$aq_cost_func,
                                 partialised_funcs$running_cost_funcs,
                                 partialised_funcs$cap_func
                               )
  )

  calculate_vpb_and_box2_tax(
    houses,
    returns_func = partial_cashflows,
    vpb_rate = parameters$vpb_rate,
    box2_rate = parameters$box2_rate
  )

}


call_calculate_vpb_and_box2_tax_transaction_only <- function(houses, parameters, funcs) {

  partialised_funcs <-
    lapply(funcs, function(v) lapply(v, function(f)
      partial(f, ... = , parameters = parameters, funcs = funcs)))

  partial_cashflows <- partial(call_get_cashflows,
                               funs = c(
                                 partialised_funcs$aq_cost_func
                               )
  )

  calculate_vpb_and_box2_tax(
    houses,
    returns_func = partial_cashflows,
    vpb_rate = parameters$vpb_rate,
    box2_rate = parameters$box2_rate
  )

}

call_calculate_vpb_and_box2_tax_sale_only <- function(houses, parameters, funcs) {
  houses$rent_or_sell = 'sell'
  houses$rent_cashflow = 0
  houses$sale_cashflow = funcs$sale_func[[1]](houses, parameters, funcs)
  call_calculate_vpb_and_box2_tax_transaction_only(
    houses, parameters, funcs
  )
}

call_calculate_vpb_and_box2_tax_keep_rent_only <- function(houses, parameters, funcs) {
  houses$rent_or_sell = 'keep_for_rent'
  houses$sale_cashflow = 0
  houses$rent_cashflow = funcs$keep_rent_func[[1]](houses, parameters, funcs)
  call_calculate_vpb_and_box2_tax_transaction_only(
    houses, parameters, funcs
  )
}

