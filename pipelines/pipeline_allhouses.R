source('dependencies.R')
devtools::load_all()

synth = T

if(synth == F){
  inputfiles = 'sensitive_input_files'
  datafiles  = 'sensitive_data_files'
  realstatshuizen <- read.delim(
    'sensitive_input_files/empirical_data_final_corrected.csv',
    sep = ';',
    dec = ","
  )
  # Parameters en huizen inladen.

  houseprice_index <- read.csv("sensitive_input_files/Woningindex.csv")

  home_values <- read.csv("sensitive_input_files/home_values_box3.csv")
  home_values_subset <- home_values %>% select(
    c("best_estimate","woz_value", "Huisnummer","Huisnummertoevoeging","Huisnummertoevoeging.1","Postcode","Straatnaam"))
  joined_rs <-realstatshuizen %>% left_join(., home_values_subset)
  realstatshuizen <- joined_rs[!duplicated(joined_rs), ]
} else{
  inputfiles = 'nonsensitive_input_files'
  datafiles = 'nonsensitive_data_files'
  realstatshuizen <- read.csv2("nonsensitive_input_files/housing_data_synthesized.csv")
  realstatshuizen$postcode = '0000SR'
  realstatshuizen$best_estimate = realstatshuizen$woz_value
}

### First map realstats data to SEO columns



realstatshuizen_subset <- realstatshuizen %>%
  tibble() %>%
  mutate(
    persoon.type = as.character(persoon.type),
    investor = case_when(
      is.na(persoon.type) ~ NA_character_,
      persoon.type == "ingeschreven_niet_natuurlijk_persoon" | persoon.type == "kadaster_niet_natuurlijk_persoon" ~ "part_box2",
      persoon.type == "ingeschreven_natuurlijk_persoon"      | persoon.type == "kadaster_natuurlijk_persoon"      ~ "part_box3",
      T ~ NA_character_
    ),
    initmarketvalrent = Huurprijs,
    woz_value = woz_value,
    best_estimate = best_estimate,
    finishing_type = Stofferingstype,
    occprate = 0.98,
    m2 = Woonoppervlakte,
    energylabel = Energielabel,
    home_type = Type.woning,
    jaar_aankoop = koopsom.koopjaar,
    aankoopwaarde = avg_price,
    m2_price = corrected_avgm2_price,
    vermogen = 101300,
    bouwjaar  = Bouwjaar,
    type = case_when(
      Type.woning== 'villa' | Type.woning== 'house' | Type.woning== 'bungalow' ~ "EGW",
      T ~ "MGW"
    ),
    gemeente = Gemeente,
    postcode = postcode,
    .keep = 'none'
  ) %>%
  filter(
    investor == "part_box3"
  ) %>%
   filter(
     woz_value > 0
   ) %>%
  group_by(bouwjaar
  ) %>%
  mutate(
    energylabel = na_if(energylabel,"")
    ) %>%
  fill(
    energylabel
  ) %>%
  group_by(
    gemeente
  ) %>%
  fill(
    energylabel
  )

# find mode
mode_energylabel <- names(sort(-table(realstatshuizen_subset$energylabel)))[1]

#fill in last NA's with the mode
realstatshuizen_subset <- realstatshuizen_subset %>%
  mutate(
    energylabel = replace(energylabel,is.na(energylabel),mode_energylabel)
  ) %>%
  ungroup() %>%
  mutate(
    teller = 1:n(),
    .before = investor
  )

# Remove bought after 2022
# remove m2 price of < 50
# remove m2 price above 12000

realstatshuizen_subset <-
  realstatshuizen_subset %>% filter(
  jaar_aankoop < 2023,
  m2_price > 50,
  m2_price < 12000
) %>%
  mutate(
    initmarketvalrent = as.double(initmarketvalrent)
  ) %>%
  mutate(
    initmarketvalrent = case_when(
      finishing_type == "upholstered" ~ initmarketvalrent * 0.94,
      finishing_type == "furnished" ~ initmarketvalrent * 0.85,
      finishing_type == "furnished or upholstered" ~ initmarketvalrent * 0.94,
      finishing_type == "shell or upholstered" ~ initmarketvalrent,
      finishing_type == "shell" ~ initmarketvalrent,
      finishing_type == "unknown" ~ initmarketvalrent
    )
  )





# Write the result to Excel for convenient viewing
wb <- loadWorkbook(paste0(inputfiles,"/modelparams_testhuizen_all.xlsx"))
writeData(wb, sheet = "voorbeeldhuizen", realstatshuizen_subset)
saveWorkbook(wb,paste0(inputfiles,"/modelparams_testhuizen_all2.xlsx"),overwrite = T)
filepath= paste0(inputfiles,'/modelparams_testhuizen_all.xlsx')


# Now for the rest of the pipeline
# Parameters en huizen inladen.
huizen <- realstatshuizen_subset ## voorbeeld investeringen
beleid <- read_xlsx(paste0(inputfiles,'/beleid_varianten_v2.xlsx'), sheet = 'beleid') ## parametrisatie van beleid

huizen$debtshare<-0
huizen$delta.dv <- 0
#etr <- as.numeric(huizen[huizen['teller'] == h, 'etr'])
huizen$exit_yield <- 0.055
huizen$finish.level <- "Medium" #afwerkingsniveau huis
huizen$init.woz <- 0#initiele woz-waarde van huis
huizen$jaar.eind <- 15 ## aantal jaren tot uitponden of doorexploitatie
huizen$n_years<- 15
#huizen$woz_waarde <- (huizen$aankoopwaarde *0.8) /huizen$cum_index


# inflatiereeksen etc inladen
parameters <- read_xlsx(paste0(inputfiles,'/modelparams_testhuizen_all2.xlsx'), sheet = 'parameters') ## tijdsreeksen zoals rentes en inflatiereeksen

# tabel leegwaarderatio=
lwrtabel <- read_xlsx(filepath, sheet = 'leegwaarderatio')
lwrtabel_oud <- read_xlsx(filepath, sheet = 'leegwaarderatio_oud')


# parameters verduurzaming
duurzaamheid_matrix <- read_xlsx(filepath, sheet = 'Verduurzaming')

# inflatiereeksen etc inladen
parameters <- read_xlsx(filepath, sheet = 'parameters') ## tijdsreeksen zoals rentes en inflatiereeksen


# Alle WWS-punten parameters staan nu op één plek.
lookup.table <- read_excel(filepath, sheet = 'wws_lookup')
wws_points <- read_excel(filepath, sheet = 'wws_puntensysteem')
cpi_index <- read_excel(filepath, sheet = 'historical_cpi')

# parameters verduurzaming

verduurzaming_starttarief <- as.numeric(duurzaamheid_matrix[12, 'A+++'])

## Referentie: Deze prijs ligt gemiddeld tussen de €50 en €200 per m2 en €300 en €500 per m3 https://www.verbouwkosten.com/verbouwing/kosten/
label_list <- c("A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G")

## Numerieke waarde opzoeken die bij een label hoort. R heeft geen dictionaries.
label_vals <- seq(length(label_list))
label_nums <- data.frame(label_list, label_vals)

# Terwille van compatibility even aparte DFs aanmaken per punten-categorie.
# Eigenlijk kun je hier beter regel 51-55 even aanpassen, en de helper functies editen.
points.a1 <- dplyr::filter(wws_points, name %in% c("m2"))
points.a4 <- dplyr::filter(wws_points, name %in% c("EGW A+++", "EGW A++", "EGW A+", "EGW A", "EGW B", "EGW C", "EGW D", "EGW E", "EGW F", "EGW G",
                                                   "MGW A+++", "MGW A++", "MGW A+", "MGW A", "MGW B", "MGW C", "MGW D", "MGW E", "MGW F", "MGW G"))
points.a56 <- dplyr::filter(wws_points, name %in% c("Low", "Medium", "High"))
points.a9 <- dplyr::filter(wws_points, name %in% c("p9.1", "p9.1b", "p9.3.i", "p9.3.ii"))

# ophoging wws punten
assign("extra.points.egw", as.numeric(wws_points[wws_points['name'] == 'extra.points.egw', 2]), envir = .GlobalEnv)
assign("extra.points.mgw", as.numeric(wws_points[wws_points['name'] == 'extra.points.mgw', 2]), envir = .GlobalEnv)

#### Kies perioden ####
huidig_jaar <- 2022

# CPI, HPI en discontovoet zijn voor iedereen (nu nog) hetzelfde:
##### Maak inflatie-index o.b.v. cpi #####
cpi_hpi_dv <- parameters %>%
  select(year, cpi, hpi, dv, loonstijging)
cpi_hpi_dv$cumcpi <- cumprod(1 + cpi_hpi_dv$cpi)
cpi_hpi_dv$cumhpi <- cumprod(1 + cpi_hpi_dv$hpi)
cpi_hpi_dv$disc_factor <- cumprod(1 + cpi_hpi_dv$dv)
cpi_hpi_dv$cum_loonstijging <- cumprod(1 + cpi_hpi_dv$loonstijging)

## CPI dubbel aanmaken, zodat overige code niet stukloopt.
cpi <- parameters %>%
  select(year, cpi)

#### Risicovrije rentes en credit rating inladen

risicovrije_rente <- read_xlsx(filepath, sheet = 'interest_rate') # Historische reeksen plus ramingen
opslag <- read_xlsx(filepath, sheet = 'rating_premium')

## Beleidsparameters
# Nagaan hoeveel beleidsvarianten er in het bestand staan
aantal_beleidsvarianten <- ncol(beleid) - 1

## WWS-puntensysteem. Voor elke woning hetzelfde, dus hoeft niet in de functie plus loop te staan
bA1 <- as.numeric(points.a1[points.a1['name'] == 'm2', 'value'])
bA91 <- as.numeric(points.a9[points.a9['name'] == 'p9.1', 'value'])
bA91b <- as.numeric(points.a9[points.a9['name'] == 'p9.1b', 'value'])




# Rewritten for-loop for speed - MS
get_beleidsparameters(beleid, 2)

#add info around energylabel
#make all numeric where possible
points.a4 <- points.a4 %>%
  separate_wider_delim(
    name, " ", names = c("type", "label")
  ) %>%
  mutate(
    delta = as.numeric(delta),
    value = as.numeric(value)
  )

houses <-
  setup.data.frame_vec(
    first.year = 2022,
    n.year = 15,
    huisaantal = nrow(huizen),
    variantaantal = 1
  ) %>%
  left_join(cpi_hpi_dv, by = 'year') %>%
  left_join(
    beleid %>%
      pivot_longer(
        !beleid,
        names_to = 'beleidsvariant'
      ) %>%
      pivot_wider(
        id_cols = beleidsvariant,
        names_from = beleid
      ) %>%
      mutate(
        beleidsvariant = as.numeric(beleidsvariant)
      ),
    by = 'beleidsvariant'
  ) %>%
  left_join(
    huizen,
    c("huis_id" = "teller")
  ) %>%
  left_join(
    label_nums %>%
      rename(
        energylabel = label_list,
        energylabelval = label_vals
      ),
    by = 'energylabel'
  )  %>%
  left_join(
    label_nums %>%
      rename(
        doel_energie_label = label_list,
        energylabeldoelval = label_vals
      ),
    by = 'doel_energie_label'
  ) %>%
  mutate(
    oppervlak = m2,
    box3.percentage = case_when(
      year == 2022 ~ belasting_box3_2022[1],
      year == 2023 ~ belasting_box3_2023[1],
      year == 2024 ~ belasting_box3_2024[1],
      T ~ belasting_box3_2025[1]
    ),
    woz_indexed = as.numeric(woz_value) * cumhpi,
    value_estimate_2023_indexed = as.numeric(best_estimate) * cumhpi,
    energie_label = case_when(
      year >= jaar_energie_label & energylabeldoelval < energylabelval ~ doel_energie_label,
      T ~ energylabel
    )
  ) %>%
  left_join(
    points.a4,
    by = c("energie_label" = "label","type" = "type")
  ) %>%
  rename(energy_label_points = value,
         wws_reform_delta = delta
  ) %>%
  left_join(
    cpi_index,
    by = c("jaar_aankoop" = "Perioden")
  ) %>%
  calculate_wws_points(.,wws_reform = 1, bA1 = bA1, bA91 = bA91, bA91b = bA91b) %>%
  correct_woz_cap(.,threshold = 187) %>%
  calculate_wws_rent(.,lookup.table) %>%
  mutate(
    huur.wws.mid = wws_rent,
    vrije.huur = 12 * initmarketvalrent
  ) %>%
  left_join(
    construct_indices('sociaal_woco') %>%
      rename( rent_increase_social_corp = percentage ),
    by = 'year'
  ) %>%
  left_join(
    construct_indices('sociaal_particulier') %>%
      rename( rent_increase_social_box3 = percentage ),
    by = 'year'
  ) %>%
  left_join(
    construct_indices('sociaal_oud') %>%
      rename( rent_increase_social_old = percentage ),
    by = 'year'
  ) %>%
  left_join(
    construct_indices('middenhuur') %>%
      rename( rent_increase_middle = percentage ),
    by = 'year'
  ) %>%
  left_join(
    construct_indices('vrijemarkt') %>%
      rename( rent_increase_free_market = percentage ),
    by = 'year'
  ) %>%
  create_rent_category(.,142,187) %>%
  get_growth_number() %>%
  index_purchase_price() %>%
  select(
    huis_id,
    beleidsvariant,
    investor,
    postcode,
    investment_year = jaar_aankoop,
    purchase_price = aankoopwaarde_indexed,
    purchase_price_unindexed = aankoopwaarde,
    init_rent = initmarketvalrent,
    woz_value,
    woz_indexed,
    oppervlak,

    construction_year = bouwjaar,
    simulation_year = year,
    energie_label,
    wws_points,
    wws_rent,
    energy_label_points,
    wws_reform_delta,
    type,

    cpi,
    cumcpi,
    hpi,
    cumhpi,
    loonstijging,

    box3.percentage,
    verhuurdersheffing_perc,
    verhuurdersheffing_grens,
    belasting_box3_bezit,
    belasting_box3_schuld,
    belasting_box3_vrijstelling,
    overdrachtsbelasting,
    opslag_middenhuur,

    huur_oud,
    rent_increase_free_market,
    rent_increase_middle,
    rent_increase_social_box3,
    rent_increase_social_corp,
    rent_increase_social_old,
    rent_category,
    growth
  ) %>%
  mutate(
    value_estimate_based_on_woz = woz_value / 0.959,
    value_estimate_indexed_based_on_woz = woz_indexed / 0.959
  ) %>%
  tibble()


## Add municipality via postal code
postal_codes_to_municipalities <-
  read_csv2('nonsensitive_input_files/postcodes_per_gemeente.csv') %>%
  select(
    postcode = PC6,
    municipality = GemNaam,
    municipality_code = GemCode
  ) %>%
  mutate(
    municipality_code = substr(municipality_code, 3, 6)
  ) %>%
  distinct()

municipalities_to_ozb <-
  read_csv2('nonsensitive_input_files/lasten_per_gemeente.csv')  %>%
  select(
    municipality_code = `Gemeente code`,
    ozb_perc = `Tarief woningen`
  )

houses <-
  houses %>%
  left_join(
    postal_codes_to_municipalities
  ) %>%
  left_join(
    municipalities_to_ozb
  )

houses$ozb_perc[is.na(houses$ozb_perc)] = 0.07845147

houses$ozb_perc = houses$ozb_perc / 100

write_feather(houses, paste0(datafiles,'/houses_realstats_', Sys.Date(), '.feather'))
