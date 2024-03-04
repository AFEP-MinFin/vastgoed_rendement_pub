#' Functions to index house purhcase price.
#'
#' @param houses Houses df
#'


#' @return A vector with as many elements as houses has rows.
#' @import assertthat dplyr
#' @export

index_purchase_price <- function(houses){
  houses <- houses %>% mutate(
    aankoopwaarde_indexed = aankoopwaarde * inverse
  )
return(houses)}
