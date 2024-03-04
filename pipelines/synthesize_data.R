source('dependencies.R')

#rflink <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
#install.packages(rflink, repos=NULL, type="source")

library(synthpop)
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

realstatshuizen_synth_set <- realstatshuizen %>%
  select(Huurprijs,
         Woonoppervlakte,
         Type.woning
         ,persoon.type,
         Bouwjaar,woz_value,
         Stofferingstype,
         Energielabel,
         avg_price,
         corrected_avgm2_price,
         woz_value,
         koopsom.koopjaar,
         Gemeente)

synt_set <- syn(realstatshuizen_synth_set,seed=123,maxfaclevels = 242)

compare(synt_set,realstatshuizen_synth_set)


synt_data <- synt_set$syn


synt_data %>%
  write.csv2('nonsensitive_input_files/housing_data_synthesized.csv')
