source('dependencies.R')

houses <- read_feather('data_files/houses_realstats_2023-07-06.feather')
wb <- loadWorkbook("input_files/modelparams_testhuizen_all2.xlsx")
realstatshuizen_subset<- read.xlsx(wb, sheet = "voorbeeldhuizen" )

single_house <- houses[houses$huis_id==1,]
interest_rates <- read.xlsx(wb, sheet = "Hypothecaire_rente" )

postal_code_coords <- sf::st_read('sensitive_data_files/geocoded_postal_codes_2023-06-20.geojson')

Sys.setenv(http_proxy = "http://proxy.bsc.rijksoverheid.nl:8080")
Sys.setenv(https_proxy = "http://proxy.bsc.rijksoverheid.nl:8080")


houses$aankoopwaarde = houses$purchase_price
houses$overdrachtsbelasting = as.numeric(houses$overdrachtsbelasting)
houses$belasting_box3_vrijstelling = as.numeric(houses$belasting_box3_vrijstelling)
houses$box3.percentage = as.numeric(houses$box3.percentage)
houses$belasting_box3_bezit = as.numeric(houses$belasting_box3_bezit)
houses$belasting_box3_schuld = as.numeric(houses$belasting_box3_schuld)

#cleansing

returns <- run_standard_scenario(
  houses = houses,
  discount_rate = 0.058,
  rent_increase = 0.02,
  upkeep_perc = 0.01,
  interest_rate = 0.05,
  debt_share = 0.5,
  exploitation_cost = 0.15,
  incidental_investment_year = 2022,
  incidental_cost = 10000,
  refinance_period = 5, # years
  middenhuur_reform = F
)

returns$cashflows
returns$stats

median(returns$stats$irr[returns$stats$irr < 0.5], na.rm = T)

hist(returns$stats$irr[returns$stats$irr < 0.5 & returns$stats$irr > 0],
     breaks = 20,
     xlab = 'IRR',
     main = "Distribution of IRR for debtshare 0.5 and interest rate 2,5%, no middenhuur")

returns_by_postal_code <-
  returns_2022$stats %>%
    group_by(beleidsvariant, postcode) %>%
    summarise(median_irr = median(irr, na.rm = T), n = n()) %>%
    mutate(median_irr = pmin(median_irr, 0.3)) %>%
    left_join(postal_code_coords %>% distinct(), by = c('postcode' = 'postal_code')) %>%
    sf::st_as_sf(.)  %>%
  sf::st_transform('EPSG:28992')

ggplot(returns_by_postal_code) +
  geom_sf(aes(color = (median_irr)))

nl_grid_with_returns <- make_nl_grid_returns(returns_by_postal_code, 500)

min_median = min(nl_grid_with_returns$median_irr, na.rm = T)
max_median = max(nl_grid_with_returns$median_irr, na.rm = T)

ggplot(nl_grid_with_returns) +
  geom_sf(aes(fill = cut(median_irr, c(-Inf, 0, 0.05, 0.1, 1)))) +
  scale_fill_viridis(
    discrete = T,
    name = 'Categorie Med. Rendement',
      na.value = 'grey90'
    ) +
  theme_void()

ggplot(nl_grid_with_returns) +
  geom_sf(aes(fill = cut(n, c(0, 5, 10, 20, 100, 1000)))) +
  scale_fill_viridis(na.value = 'grey90', discrete = T, name = 'Aantal woningen per cel') +
  theme_void()

postal_code_coords


# count number of houses in area
houses_within = st_is_within_distance(postal_code_coords, sparse = T, dist = 2500)
houses_count = data.frame(count = sapply(test_within, length) - 1, postal_code = postal_code_coords$postal_code)

returns_by_postal_code %>% tibble() %>% select(-geometry) %>%
  left_join(houses_count, by = c('postcode' = 'postal_code')) %>% distinct() %>%
  ggplot(aes(x = count, y = median_irr)) + geom_point() + stat_smooth()
