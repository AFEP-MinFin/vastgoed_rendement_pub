#' Functions for rent
#'
#' @param houses Houses df
#'


#' @return A vector with as many elements as houses has rows.
#' @import assertthat dplyr
#' @export

rent_constant <- function(houses) {
  assert_that( all(has_name(houses, 'init_rent')) , msg = 'Required variable not available' )
  return(tibble(rent_income = houses$init_rent * 12))
}

#' @return A vector with as many elements as houses has rows.
#' @import assertthat dplyr
#' @export

rent_max_wws <- function(houses, parameters, funcs) {
  assert_that( all(has_name(houses, 'wws_rent')) , msg = 'Required variable not available' )

  houses %>% mutate(
    rent_income = case_when(
      as.character(rent_category) != 'free' ~ as.numeric(wws_rent) * cumcpi,
      T ~ call_calculate_rent_all_houses(houses, parameters, funcs)$rent_income
    )
  ) %>%
    select(rent_income)
}

#' @return A vector with as many elements as houses has rows.
#' @import assertthat dplyr
#' @export

rent_very_high <- function(houses, parameters, funcs) {
  assert_that( all(has_name(houses, 'init_rent')) , msg = 'Required variable not available' )
  return(tibble(rent_income = 1000000))
}

#' rent_constant_increase
#'
#' @param houses Houses df
#' @param increase_factor Percentage increase per year (ie 0.02)

#' @export
rent_constant_increase <- function(houses, increase_factor = 0.02) {
  assert_that( all(has_name(houses, c('init_rent', 'huis_id', 'beleidsvariant'))) , msg = 'Required variable not available' )

  houses$rent_increase_factor = increase_factor
  houses <- houses %>%
    group_by(huis_id, beleidsvariant) %>%
    mutate(
      cum_rent_increase_factor = cumprod(1+rent_increase_factor)
    )

  return(tibble(rent_income = houses$init_rent * 12 * houses$cum_rent_increase_factor))
}


#' Calculate rent for all housing sector
#'
#' @param houses Houses df
#' @param growth_newcomer Rent increase when occupant changes
#' @param growth_stayer Rent increase with same occupant
#' @param mutation_grade Mutation grade of occupants
#'
#'@export
calculate_rent_all_houses <- function(houses,growth_newcomer,mutation_grade, parameters){

  splitted_houses <- houses %>% group_split(huis_id,beleidsvariant)
  houses['rent_income'] <-as.numeric(unlist(lapply(splitted_houses, function(x){
    .calculate_rent_single_house(singlehouse = x,
                                grow_index_newcomer = growth_newcomer,
                                grow_index_stay = x$growth,
                                mutation_grade = mutation_grade,
                                wws_dwingend = parameters$wws_dwingend)})))
  return(houses['rent_income'])
}

calculate_rent_all_houses_memoise <- memoise::memoise(calculate_rent_all_houses)

#'@export
call_calculate_rent_all_houses <- function(houses, parameters, funcs) {
  calculate_rent_all_houses_memoise(
    houses = houses,
    parameters = parameters,
    growth_newcomer = .086,
    mutation_grade = .15
  )
}


#'@export
.calculate_rent_single_house<- function(singlehouse,grow_index_stay,grow_index_newcomer,mutation_grade, wws_dwingend){
  occupancy_matrix <- .get_occupancy_matrix(houses = singlehouse,mutation_grade = mutation_grade )
  rent <- .calculate_rent_matrix(
    .construct_stayer_rent(singlehouse,grow_index_stay),
    .construct_newcomer_rent(singlehouse,grow_index_newcomer, wws_dwingend),
    occupancy_matrix)
  return(rent)

}

.get_occupancy_matrix <- function(houses,mutation_grade){
  n_years<-length(unique(houses$simulation_year))-1
  occupancy_matrix<-matrix(0,nrow=n_years+1,ncol=n_years+1)
  occupancy_matrix[1,1]<-1
  for (i in 1:(n_years)){
    for (j in (i+1):(n_years+1)) {
      occupancy_matrix[i,j] <- occupancy_matrix[i,j-1] * (1-mutation_grade)
    }
    occupancy_matrix[i+1,i+1] <- mutation_grade
  }
  return(occupancy_matrix)
}



#' Rent category
#'
#' Assign a rent sector a property is in in a given year.
#'
#' @param houses Houses df
#' @param points_social_boundary Point up to which social housing
#' @param points_middle_boundary Point up to which 'middenhuur'
#'
#'@export
create_rent_category <- function(houses, points_social_boundary, points_middle_boundary){
  houses %>% mutate(
    rent_category = case_when(wws_points <= points_social_boundary ~ "social",
                              wws_points <= points_middle_boundary ~ "middle",
                              T ~ "free")
  )
}

#'
#'
#' Get rental growth
#'
#' Apply the correct rent increase factor
#'
#' @param houses Houses df
#' @export
get_growth_number<- function(houses){
  houses %>% mutate(
    growth = case_when(
      rent_category == 'social' & (investor == 'part_box2' | investor =='part_box3') & huur_oud == 'nee' ~ rent_increase_social_box3,
      rent_category == 'social' & (investor == 'inst_vpb' | investor =='woco') & huur_oud == 'nee'~ rent_increase_social_corp,
      rent_category == 'social' & (investor == 'part_box2' | investor =='part_box3') & huur_oud == 'ja' ~ rent_increase_social_old,
      rent_category == 'social' & (investor == 'inst_vpb' | investor =='woco') & huur_oud == 'ja'~ rent_increase_social_old,
      rent_category == 'middle'  ~ rent_increase_middle,
      rent_category == 'free' ~ rent_increase_free_market
    )
  )
}




#'@export
.get_start_rent <- function(singlehouse,wws_dwingend){
  singlehouse %>% mutate(
    start_rent = case_when(
      as.character(rent_category) != 'free' & wws_dwingend == 'ja' ~ as.numeric(pmin(init_rent * 12, wws_rent)),
      T ~ as.numeric(init_rent) * 12
    )
  ) %>%
    select(start_rent)
}


#'@export
.construct_newcomer_rent <- function(singlehouse,newcomer_index, wws_dwingend){
  n_years <- length(unique(singlehouse$simulation_year))-1
  rent_matrix <- matrix(0, n_years + 1, n_years + 1)
  diag(rent_matrix) <- unlist(.get_start_rent(singlehouse, wws_dwingend)) * (1+newcomer_index)
  return(rent_matrix)
}

#'@export
.construct_stayer_rent <- function(singlehouse,index){
  rent_increase_stayers<-t(matrix((1+index), nrow = length(unique(singlehouse$simulation_year)),ncol = length(unique(singlehouse$simulation_year))))
  diag(rent_increase_stayers) <- 1
  rent_increase_stayers[lower.tri(rent_increase_stayers)] <- 1
  rent_increase_stayers <- t(apply(rent_increase_stayers,1,cumprod))
  rent_increase_stayers[lower.tri(rent_increase_stayers)] <- 0
  return(rent_increase_stayers)
}


#'@export
.calculate_rent_matrix<-function(stayer_matrix,rent_matrix,occupancy_matrix){
  rent_matrix_filled <- stayer_matrix * diag(rent_matrix)
  return(colSums(rent_matrix_filled * occupancy_matrix))
}




