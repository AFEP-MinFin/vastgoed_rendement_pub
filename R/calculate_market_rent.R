#' Calculate market rent
#'
#' @param houses Houses df
#' @param market_rent_table

#'
#' @return a vector of marketrent per year
#' @import jrvFinance
#' @export
calculate_market_rent <- function(houses,market_rent_table){
  markthuur_tabel <- read_xlsx('input_files/modelparams_testhuizen_all2.xlsx', sheet = 'Markthuur')
  fWOZ_tabel <- subset(markthuur_tabel, markthuur_tabel$parameter == 'fWOZ')
  fBouwjaar_tabel <- subset(markthuur_tabel, markthuur_tabel$parameter == 'fBouwjaar')
  fType_tabel <- subset(markthuur_tabel, markthuur_tabel$parameter == 'fType')
  fRegio_tabel <- subset(markthuur_tabel, markthuur_tabel$parameter == 'fRegio')
  fMarkt_tabel <- subset(markthuur_tabel, markthuur_tabel$parameter == 'fMarkt')
  huur_ref <- as.numeric(markthuur_tabel[markthuur_tabel$parameter == 'huur_ref', 'parameter_waarde'])

  houses <- houses %>% mutate(
    fWOZ = case_when(woz_indexed < as.numeric(fWOZ_tabel$variabele_waarde[1]) ~ as.numeric(fWOZ_tabel$parameter_waarde[1]) ,
                     woz_indexed <  as.numeric(fWOZ_tabel$variabele_waarde[2]) ~ as.numeric(fWOZ_tabel$parameter_waarde[2]),
                     woz_indexed <  as.numeric(fWOZ_tabel$variabele_waarde[3]) ~ as.numeric(fWOZ_tabel$parameter_waarde[3]),
                     woz_indexed < as.numeric(fWOZ_tabel$variabele_waarde[4]) ~ as.numeric(fWOZ_tabel$parameter_waarde[4]),
                     woz_indexed <  as.numeric(fWOZ_tabel$variabele_waarde[5]) ~ as.numeric(fWOZ_tabel$parameter_waarde[5]),
                     woz_indexed <  as.numeric(fWOZ_tabel$variabele_waarde[6]) ~ as.numeric(fWOZ_tabel$parameter_waarde[6]),
                     woz_indexed < as.numeric(fWOZ_tabel$variabele_waarde[7]) ~ as.numeric(fWOZ_tabel$parameter_waarde[7]),
                     woz_indexed <  as.numeric(fWOZ_tabel$variabele_waarde[8]) ~ as.numeric(fWOZ_tabel$parameter_waarde[8]),
                     woz_indexed >=  as.numeric(fWOZ_tabel$variabele_waarde[9]) ~ as.numeric(fWOZ_tabel$parameter_waarde[9]),
                     ),
    fBouwjaar = case_when(construction_year < as.numeric(fBouwjaar_tabel$variabele_waarde[1]) ~ as.numeric(fBouwjaar_tabel$parameter_waarde[1]) ,
                          construction_year <  as.numeric(fBouwjaar_tabel$variabele_waarde[2]) ~ as.numeric(fBouwjaar_tabel$parameter_waarde[2]),
                          construction_year <  as.numeric(fBouwjaar_tabel$variabele_waarde[3]) ~ as.numeric(fBouwjaar_tabel$parameter_waarde[3]),
                          construction_year < as.numeric(fBouwjaar_tabel$variabele_waarde[4]) ~ as.numeric(fBouwjaar_tabel$parameter_waarde[4]),
                          construction_year <  as.numeric(fBouwjaar_tabel$variabele_waarde[5]) ~ as.numeric(fBouwjaar_tabel$parameter_waarde[5]),
                          construction_year < as.numeric(fBouwjaar_tabel$variabele_waarde[6]) ~ as.numeric(fBouwjaar_tabel$parameter_waarde[6]),
                          construction_year >= as.numeric(fBouwjaar_tabel$variabele_waarde[7]) ~ as.numeric(fBouwjaar_tabel$parameter_waarde[7])),
    fType = case_when (type == "EGW"~ 0.973,
                        type == "MGW" ~1),
    fRegio = 1,
    fMarkt = 1.063
  ) %>% mutate(
    free_rent = (oppervlak/((0.0125 * oppervlak) ** 0.6796)) * huur_ref * fWOZ * fBouwjaar * fType * fRegio * fMarkt
  )
  return(houses)
}
