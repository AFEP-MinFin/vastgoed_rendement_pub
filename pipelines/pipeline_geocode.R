library(httr)
library(sf)

realstatshuizen <- read.delim(
  'input_files/empirical_data_final.csv',
  sep = ';',
  dec = ","
)

geocode_postal_code <- function(postal_code, api_key) {
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"

  params <- list(
    address = paste0(postal_code, ", Netherlands"),
    key = api_key
  )

  response <- GET(base_url, query = params)

  if (status_code(response) == 200) {
    data <- content(response, "parsed")

    if (data$status == "OK" && length(data$results) > 0) {
      lat <- data$results[[1]]$geometry$location$lat
      lon <- data$results[[1]]$geometry$location$lng

      coordinates <- data.frame(lon = lon, lat = lat, postal_code = postal_code)
      sf_object <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326)

      return(sf_object)
    }
  }

  return(NULL)
}

# Example usage
api_key <- "-"

results <- purrr::map(realstatshuizen$postcode, geocode_postal_code, api_key = api_key)
st_write(bind_rows(results), paste0('data_files/geocoded_postal_codes_', Sys.Date(), '.geojson'))
