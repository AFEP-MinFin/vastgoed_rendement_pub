#' Functions for investments and upkeep
#'
#' @param houses Houses df
#' @param upkeep_perc Percentage of home value spent on upkeep yearly (eg 0.01)
#'
#' @return A vector with as many elements as houses has rows.
#' @import assertthat
#' @export

investment_constant_upkeep <- function(houses, upkeep_perc) {
  assert_that( all(has_name(houses, 'purchase_price')) , msg = 'Required variable not available' )
  return(tibble(upkeep_payments = -1 * houses$purchase_price * upkeep_perc))
}

#' Exploitation cost
#'
#' @param houses Houses df
#' @param exploitation_cost Percentage of rent spent on exploitation  (eg 0.2)
#'
#' @return A vector with as many elements as houses has rows.
#' @import assertthat
#' @export

investment_exploitation_cost <- function(houses, rent_func, exploitation_cost_perc) {
  rent_income <- rent_func(houses)[[1]]
  return(tibble(upkeep_payments = -1 * rent_income * exploitation_cost_perc))
}

call_investment_exploitation_cost <- function(houses, parameters, funcs) {
  investment_exploitation_cost(
    houses = houses,
    rent_func = partial(.f = funcs$cap_func[[1]], ... = , parameters = parameters, funcs = funcs),
    exploitation_cost = parameters$exploitation_cost
  )
}

#' Incidental cost
#'
#' @param houses Houses df
#' @param incidental_cost number of init cost of rent spent on exploitation  (eg 0.2)
#' @param investment_year Modeled year of investment
#'
#' @return A vector with as many elements as houses has rows.
#' @import assertthat
#' @export

investment_incidental_cost <- function(houses, incidental_cost,investment_year) {
  return(tibble(incidental_investment = case_when(
    houses$simulation_year == investment_year ~ -1 * incidental_cost,
    T ~ 0)
    ))
}

call_investment_incidental_cost <- function(houses, parameters, funcs) {
  investment_incidental_cost(
    houses = houses,
    incidental_cost = parameters$incidental_cost,
    investment_year = parameters$incidental_investment_year
  )
}


#' Fixed cost handboek
#'
#' @return A vector with as many elements as houses has rows.
#' @import assertthat
#' @export

investment_upkeep_hmw <- function(houses, parameters, funcs) {
  houses %>%
    mutate(
      ozb = ozb_perc * woz_indexed,
      other_tax_and_insurance = 0.0007 * woz_indexed,
      costs_upkeep = case_when(
        type == 'EGW' & construction_year < 1940 & oppervlak < 40 ~ 1467 + 508.9,
        type == 'EGW' & construction_year < 1940 & oppervlak < 60 ~ 1652 + 508.9,
        type == 'EGW' & construction_year < 1940 & oppervlak < 80 ~ 1773 + 508.9,
        type == 'EGW' & construction_year < 1940 & oppervlak < 100 ~ 1893 + 508.9,
        type == 'EGW' & construction_year < 1940 & oppervlak < 120 ~ 1946 + 508.9,
        type == 'EGW' & construction_year < 1940                   ~ 1998 + 508.9,
        type == 'EGW' & construction_year < 1960 & oppervlak < 40 ~ 1441 + 508.9,
        type == 'EGW' & construction_year < 1960 & oppervlak < 60 ~ 1612 + 508.9,
        type == 'EGW' & construction_year < 1960 & oppervlak < 80 ~ 1725 + 508.9,
        type == 'EGW' & construction_year < 1960 & oppervlak < 100 ~ 1838 + 508.9,
        type == 'EGW' & construction_year < 1960 & oppervlak < 120 ~ 1888 + 508.9,
        type == 'EGW' & construction_year < 1960                   ~ 1939 + 508.9,
        type == 'EGW' & construction_year < 1975 & oppervlak < 40 ~ 1444 + 508.9,
        type == 'EGW' & construction_year < 1975 & oppervlak < 60 ~ 1608 + 508.9,
        type == 'EGW' & construction_year < 1975 & oppervlak < 80 ~ 1724 + 508.9,
        type == 'EGW' & construction_year < 1975 & oppervlak < 100 ~ 1846 + 508.9,
        type == 'EGW' & construction_year < 1975 & oppervlak < 120 ~ 1907 + 508.9,
        type == 'EGW' & construction_year < 1975                   ~ 1967 + 508.9,
        type == 'EGW' & construction_year < 1990 & oppervlak < 40 ~ 1468 + 508.9,
        type == 'EGW' & construction_year < 1990 & oppervlak < 60 ~ 1620 + 508.9,
        type == 'EGW' & construction_year < 1990 & oppervlak < 80 ~ 1730 + 508.9,
        type == 'EGW' & construction_year < 1990 & oppervlak < 100 ~ 1847 + 508.9,
        type == 'EGW' & construction_year < 1990 & oppervlak < 120 ~ 1906 + 508.9,
        type == 'EGW' & construction_year < 1990                   ~ 1965 + 508.9,
        type == 'EGW' & construction_year < 2005 & oppervlak < 40 ~ 1525 + 508.9,
        type == 'EGW' & construction_year < 2005 & oppervlak < 60 ~ 1692 + 508.9,
        type == 'EGW' & construction_year < 2005 & oppervlak < 80 ~ 1811 + 508.9,
        type == 'EGW' & construction_year < 2005 & oppervlak < 100 ~ 1943 + 508.9,
        type == 'EGW' & construction_year < 2005 & oppervlak < 120 ~ 2001 + 508.9,
        type == 'EGW' & construction_year < 2005                   ~ 2064 + 508.9,
        type == 'EGW' &                            oppervlak < 40 ~ 1634 + 508.9,
        type == 'EGW' &                            oppervlak < 60 ~ 1812 + 508.9,
        type == 'EGW' &                            oppervlak < 80 ~ 1939 + 508.9,
        type == 'EGW' &                            oppervlak < 100 ~  2073 + 508.9,
        type == 'EGW' &                            oppervlak < 120 ~ 2139 + 508.9,
        type == 'EGW'                                              ~ 2205 + 508.9,
        type == 'MGW' & construction_year < 1940 & oppervlak < 40 ~ 1254 + 499.4,
        type == 'MGW' & construction_year < 1940 & oppervlak < 60 ~ 1344 + 499.4,
        type == 'MGW' & construction_year < 1940 & oppervlak < 80 ~ 1475 + 499.4,
        type == 'MGW' & construction_year < 1940 & oppervlak < 100 ~ 1652 + 499.4,
        type == 'MGW' & construction_year < 1940 & oppervlak < 120 ~ 1813 + 499.4,
        type == 'MGW' & construction_year < 1940                   ~ 1929 + 499.4,
        type == 'MGW' & construction_year < 1960 & oppervlak < 40 ~ 1297 + 499.4,
        type == 'MGW' & construction_year < 1960 & oppervlak < 60 ~ 1376 + 499.4,
        type == 'MGW' & construction_year < 1960 & oppervlak < 80 ~ 1494 + 499.4,
        type == 'MGW' & construction_year < 1960 & oppervlak < 100 ~ 1655 + 499.4,
        type == 'MGW' & construction_year < 1960 & oppervlak < 120 ~ 1801 + 499.4,
        type == 'MGW' & construction_year < 1960                   ~ 1905 + 499.4,
        type == 'MGW' & construction_year < 1975 & oppervlak < 40 ~ 1318 + 499.4,
        type == 'MGW' & construction_year < 1975 & oppervlak < 60 ~ 1421 + 499.4,
        type == 'MGW' & construction_year < 1975 & oppervlak < 80 ~ 1558 + 499.4,
        type == 'MGW' & construction_year < 1975 & oppervlak < 100 ~ 1731 + 499.4,
        type == 'MGW' & construction_year < 1975 & oppervlak < 120 ~ 1892 + 499.4,
        type == 'MGW' & construction_year < 1975                   ~ 2014 + 499.4,
        type == 'MGW' & construction_year < 1990 & oppervlak < 40 ~ 1357 + 499.4,
        type == 'MGW' & construction_year < 1990 & oppervlak < 60 ~ 1457 + 499.4,
        type == 'MGW' & construction_year < 1990 & oppervlak < 80 ~ 1589 + 499.4,
        type == 'MGW' & construction_year < 1990 & oppervlak < 100 ~ 1754 + 499.4,
        type == 'MGW' & construction_year < 1990 & oppervlak < 120 ~ 1909 + 499.4,
        type == 'MGW' & construction_year < 1990                   ~ 2026 + 499.4,
        type == 'MGW' & construction_year < 2005 & oppervlak < 40 ~ 1460 + 499.4,
        type == 'MGW' & construction_year < 2005 & oppervlak < 60 ~ 1586 + 499.4,
        type == 'MGW' & construction_year < 2005 & oppervlak < 80 ~ 1705 + 499.4,
        type == 'MGW' & construction_year < 2005 & oppervlak < 100 ~ 1876 + 499.4,
        type == 'MGW' & construction_year < 2005 & oppervlak < 120 ~ 2035 + 499.4,
        type == 'MGW' & construction_year < 2005                   ~ 2154 + 499.4,
        type == 'MGW' &                            oppervlak < 40 ~ 1565 + 499.4,
        type == 'MGW' &                            oppervlak < 60 ~ 1687 + 499.4,
        type == 'MGW' &                            oppervlak < 80 ~ 1837 + 499.4,
        type == 'MGW' &                            oppervlak < 100 ~  2020 + 499.4,
        type == 'MGW' &                            oppervlak < 120 ~ 2188 + 499.4,
        type == 'MGW'                                              ~ 2315 + 499.4,
      ),
      costs_upkeep_indexed = costs_upkeep * cumcpi,
      fixed_cost_hmw = -1 * (ozb + other_tax_and_insurance + costs_upkeep_indexed)
    ) %>%
    select(
      fixed_cost_hmw
    )
}
