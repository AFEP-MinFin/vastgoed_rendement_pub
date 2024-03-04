#' calculate_wws_points
#'
#' @param houses -
#'
#' @param delta_value -
#' @param bA1 -
#' @param bA91 -
#' @param bA91b -
#'
#' @export
calculate_wws_points<-function(houses, wws_reform, bA1 = 1 , bA91 = 11041, bA91b = 172, extra.points.egw = 31, extra.points.mgw = 17){
  houses <- houses %>%
    .base_points_calculate(.,wws_reform, bA1) %>%
    .points_correct_egwmgw(., extra.points.egw, extra.points.mgw) %>%
    mutate(points_woz = houses$woz_indexed / (bA91) + houses$woz_indexed / houses$oppervlak / (bA91b) ) %>%
    mutate(wws_points = points_exc_woz + points_woz)
  return(houses)
}

#add points based on type of energy label
#' points_correct_egwmgew
#'
#' @param houses -
#'
#' @export
.points_correct_egwmgw<- function(houses,extra.points.egw,extra.points.mgw){
  houses <- houses %>% mutate(points_exc_woz = case_when(
    type == "EGW" ~
        points_counter + extra.points.egw,
        T ~ points_counter + extra.points.mgw)
  )
  return (houses)
}

#'Calculate on surface and energylabel points.
#' @param houses -
#'
#' @param wws_reform -
#' @param bA1 -
#'
#' @export
.base_points_calculate<- function(houses,wws_reform, bA1){
  houses <- houses %>% mutate(points_counter = case_when(
    wws_reform == 0 ~  bA1 * oppervlak + energy_label_points,
    T ~ bA1 * oppervlak + energy_label_points + wws_reform_delta)
  )
  return(houses)
}
