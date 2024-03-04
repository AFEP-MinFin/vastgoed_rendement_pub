#' Functions for creating uniform shell rental ask price
#'
#' @param houses Houses df
#' @param correction_upholstered Percentage correction to define uniform house rental. 0.95 -> 95% of rental price is for shell.
#' @param correction_upholstered Percentage correction to define uniform house rental. 0.90 -> 95% of rental price is for shell.

#'
#' @return A vector with as many elements as houses has rows.
#' @import assertthat
#' @export
get_uniform_rent_price <- function(houses,correction_upholstered = 0.94, correction_furnished = 0.85){
  houses <- houses %>%
    mutate(
      initmarketvalrent= case_when(finishing_type == "upholstered" ~ initmarketvalrent * correction_upholstered,
                finishing_type == "furnished" ~ initmarketvalrent * correction_furnished,
                finishing_type == "furnished or upholstered" ~ initmarketvalrent * correction_upholstered,
                finishing_type == "shell or upholstered" ~ initmarketvalrent,
                finishing_type == "shell" ~ initmarketvalrent,
                finishing_type == "unknown" ~ initmarketvalrent
                )
    )
  return(houses)
}
