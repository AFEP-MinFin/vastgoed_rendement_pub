#' Calculate Box3  tax , based,  assets and debt. Uses leegwaarde and vrijstellingsfactor to come to correct

#'
#' @return A set of columns used to determine pre-post tax cashflows
#' @export


#' @param houses Houses df
#'
#' @param debt_share Percentage of home value as mortgage debt
#' @param lwr 'nieuw' or 'oud' for old or new lwr system
#' @param investor Investor type string
#' @param forfait_schijf2_oud Assumed return in 2022 box 3
#' @param box3_oud 'nieuw' or 'oud' for 2023 and after or 2022 system
#' @param vermogen Wealth outside of property
#'
#' @export
#' @import dplyr purrr

calculate_box3 <- function(
    houses,
    debt_share,
    rent_func,
    transfer_tax = 0.104,
    lwr = 'nieuw',
    box3_percentage_2025onwards = 0.34,
    investor = 'part_box3',
    forfait_schijf2_oud = 0.0467,
    box3_oud = 'nee',
    vermogen = 100000
    ) {

  houses$rent <- rent_func(houses)[[1]]

  houses %>%
    #.overwrite_percentage_box3(box3_percentage_2025onwards) %>%
    calculate_empty_house_value(lwr) %>%
    calculate_debt(debt_share) %>%
    calculate_exemption_factor(vermogen) %>%
    mutate(
      #Het rendement van bank- en spaartegoeden en contant geld telt u op bij het rendement van beleggingen en andere bezittingen. Het totaal vermindert u met het rendement op de aftrekbare schulden. Dit is uw belastbaar rendement.
      # We nemen hier aan dat schuld alleen de schuld van de belegging is. Vermogen is overig spaargeld.
      box3_tax_return = empty_house_value * belasting_box3_bezit - (debt-3400) * belasting_box3_schuld,
      #Daarnaast berekent u uw vermogen. Dat bestaat uit het totaal van de soorten vermogens die u hebt. Dus uw bezittingen min uw schulden. En de schulden vermindert u eerst met de drempel. Dit is uw rendementsgrondslag.
      tax_basis =  empty_house_value - (debt-3400),
      #Uw vermogen vermindert u met het heffingsvrij vermogen. Dan hebt u de grondslag sparen en beleggen. De vrijstellingsfactor is het deel
      tax_basis_house = (tax_basis-belasting_box3_vrijstelling*exemption_factor),
      #U deelt uw grondslag sparen en beleggen door de rendementsgrondslag en vermenigvuldigt dit met 100. Rond af op 2 decimalen achter de komma. Dit percentage is uw aandeel in de rendementsgrondslag.
      return_base_house= tax_basis_house/tax_basis,

      box3_tax_new = box3_tax_return  * return_base_house * box3.percentage,
      box3_tax_oud = (.31 * .0436 * (empty_house_value -  debt))
    ) %>%
    mutate(
      box3_tax =
        case_when(box3_oud=="ja" & investor =="part_box3" ~  -box3_tax_oud,
                  box3_oud=="nee" & investor =="part_box3" ~  -box3_tax_new)
    ) %>%
    select(
      box3_tax
    )
}

calculate_box3_mem <- memoise::memoise(calculate_box3)

call_calculate_box3 <- function(houses, parameters, funcs) {
  calculate_box3_mem(
    houses = houses,
    box3_percentage_2025onwards = parameters$box3_percentage_2025onwards,
    debt_share = parameters$debt_share,
    box3_oud = parameters$box3_oud,
    rent_func = partial(funcs$cap_func[[1]], parameters = parameters, funcs = funcs)
  )
}

call_calculate_box3_transaction_only <- function(houses, parameters, funcs) {
  return(
    tibble(box_3_transaction_tax = rep(0, nrow(houses)))
  )
}

calculate_empty_house_value <- function(houses, lwr) {
  houses %>%
    mutate(
      empty_house_value_ratio = case_when(
        lwr =="nieuw" & rent / woz_indexed < 0.01 ~ 0.73,
        lwr =="nieuw" & rent / woz_indexed  < 0.02 ~ 0.79,
        lwr =="nieuw" & rent / woz_indexed  < 0.03 ~ 0.84,
        lwr =="nieuw" & rent / woz_indexed  < 0.04 ~ 0.9,
        lwr =="nieuw" & rent / woz_indexed  < 0.05 ~ 0.95,
        lwr =="nieuw" ~ 1,
        lwr =="oud" & rent / woz_indexed < 0.01 ~ 0.45,
        lwr =="oud" & rent / woz_indexed < 0.02 ~ 0.51,
        lwr =="oud" & rent / woz_indexed  < 0.03 ~ 0.56,
        lwr =="oud" & rent / woz_indexed  < 0.04 ~ .62,
        lwr =="oud" & rent / woz_indexed  < 0.05 ~.67,
        lwr =="oud" & rent / woz_indexed  < 0.06 ~.73,
        lwr =="oud" & rent / woz_indexed  < 0.07 ~.78,
        lwr =="oud"  ~.85),
      empty_house_value = empty_house_value_ratio * woz_indexed
    )
}

calculate_exemption_factor <- function(houses, vermogen){
  houses %>% mutate(
    exemption_factor= pmax(0,  (empty_house_value-(debt-3400)) / pmax(0,(vermogen * cumcpi + empty_house_value-(debt-3400))))
  )
}


.overwrite_percentage_box3 <- function(houses,box3_percentage_2025onwards = 0.34){
  houses %>% mutate(
  box3.percentage = if_else(simulation_year >= 2025,  box3_percentage_2025onwards, as.numeric(box3.percentage))
                              )
}

