#' Calculate returns given a new policy
#'

#' @param middenhuur_bump_percentage percentage to increase wws rent
#' @param houses the generic houses dataframe
#'
#' @return houses df with newly calculated wws_rent given the percentage. Only applied to middenhuur houses.
#' @export
middenhuur_rent_bump <- function(houses,middenhuur_bump_percentage){
  houses <- houses %>%
    mutate( wws_rent = case_when(rent_category == 'middle' ~ wws_rent * (1 + middenhuur_bump_percentage),
                                 T ~ wws_rent
                                 )
  )
  return(houses)
}

#'@export
middenhuur_points_bump <- function(houses,middenhuur_bump_points,lookup.table){
  houses <- houses %>%
    mutate( wws_points = case_when(rent_category == 'middle' ~ wws_points + middenhuur_bump_points ,
                                 T ~ wws_points)
    ) %>%
    calculate_wws_rent(.,lookup.table = lookup.table) %>%
    select(-c(value_rent, note)) %>%
    create_rent_category(.,142,187) %>%
    get_growth_number()

  return(houses)
}
