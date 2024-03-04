#' Make Geo Grid object with returns
#'
#' @param returns_by_postal_code Returns by postal code
#' @param num_hex Number of cells in grid
#'
#' @import sf
#'
#' @return Geo object
#' @export
#'
make_nl_grid_returns <- function(returns_by_postal_code, num_hex) {

  # Load borders
  grenzen_nl <-
    sf::st_read(
  'sensitive_input_files/zipcodecoordinates.geojson') %>%
    sf::st_transform('EPSG:28992')

  hex_size <- sqrt(sf::st_area(sf::st_union(grenzen_nl)) / num_hex)

  ### Create grid
  nl_grid <-
    sf::st_make_grid(
      grenzen_nl,
      cellsize = c(as.numeric(hex_size), as.numeric(hex_size)),
      what = "polygons"
    ) %>%
    sf::st_as_sf(.)

  nl_grid$grid_id <- 1:nrow(nl_grid)
  nl_grid <-
    sf::st_intersection(
      sf::st_simplify(nl_grid, dTolerance = 1000),
      sf::st_simplify(grenzen_nl, dTolerance = 1000)
    )

  # Join grid with returns
  nl_grid_with_returns <-
    sf::st_join(nl_grid, returns_by_postal_code %>% sf::st_as_sf(.), left = T) %>%
    group_by(grid_id) %>%
    summarise(median_irr = median(median_irr, na.rm = T),
              n = sum(n)) %>%
    ungroup() %>%
    mutate(median_irr = if_else(n > 2, median_irr, NA_real_))

  return(nl_grid_with_returns)
}
