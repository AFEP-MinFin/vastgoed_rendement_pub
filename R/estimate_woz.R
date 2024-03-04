#' Functions for estimating WOZ value in 2022
#'
#' @param houses Houses df
#'


#' @return A vector with as many elements as houses has rows.
#' @import assertthat dplyr
#' @export
calculate_house_price_index <- function(houseprice_index,houses){
  houseprice_index <- houseprice_index %>%
    .construct_index_all_years() %>%
    .calculate_compounded_increase() %>%
    .redefine_municipalities()

  houses <- left_join(x=houses,
              y= houseprice_index,
              join_by(gemeente == municipality, jaar_aankoop == year)
              )
  return(houses)
}

.construct_index_all_years <- function(houseprice_index){
  years <- unique(houseprice_index$Perioden)
  municipalities <- unique(houseprice_index$Regio)
  combinations <- expand.grid(year = years,municipality = municipalities)
  df <- data.frame(combinations)

  df2 <-  df %>%
        left_join(
                  houseprice_index[is.na(houseprice_index$Regio),],
                  by(year = Perioden)
                  ) %>%
        left_join(houseprice_index,
                  by(year = Perioden, municipality = Regio)
                  )
  df3 <- df2 %>% mutate(
    index = if_else(is.na(per.woningstijging.x),
                    per.woningstijging.y,
                    per.woningstijging.x
                    )
  ) %>%
    select(c(year,municipality,index))
  return (df3)
}


.redefine_municipalities <- function(houseprice_index){
  houseprice_index <- houseprice_index %>% mutate(
    municipality = case_when(   municipality == "'s-Gravenhage (gemeente)" ~ "Den Haag",
                         municipality == "Groningen (gemeente)" ~ "Groningen",
                         municipality == "Utrecht (gemeente)" ~ "Utrecht",
                         municipality == "'s-Hertogenbosch" ~ "Den Bosch",
                         municipality == "Rijswijk (ZH.)" ~ "Rijswijk",
                         municipality == "Middelburg (Z.)" ~ "Middelburg",
                         municipality == "Hengelo (O.)" ~ "Hengelo",
                         municipality == "Beek (L.)" ~ "Beek",
                         municipality == "Laren (NH.)" ~ "Laren",
                         municipality == 'Súdwest-Fryslân' ~ 'SÃƒÂºdwest-FryslÃƒÂ¢n',
                        T ~ municipality
    )
  )
  return(houseprice_index)
}

.calculate_compounded_increase <- function(houseprice_index){
  houseprice_index$index[is.na(houseprice_index$index)] <- 0
  houseprice_index$index[houseprice_index$tyear == 2022] <- 0
  houseprice_index<-houseprice_index %>%
    group_by(municipality) %>%
    arrange(municipality,desc(year)) %>%
    mutate(
      cum_index  = 1/(cumprod(1+index))
    )
  return(houseprice_index)
}
