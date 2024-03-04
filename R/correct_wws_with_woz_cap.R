#'calculate capped points if condition is met. Overwrite the woz_points with corrected amount of points
#' calc_capped_points
#'
#' @param houses
#' @param threshold
#'
#' @export
correct_woz_cap<-function(houses, threshold = 142, bA93i = 0.33){
  houses <- houses %>%
    mutate( corrected_woz_points =
              case_when(
                wws_points > threshold & points_woz/wws_points > bA93i  ~ points_exc_woz*bA93i/(1-bA93i),
                T ~ points_woz)
    ) %>%
    mutate(wws_points = floor(points_exc_woz + corrected_woz_points))
  return(houses)
}
