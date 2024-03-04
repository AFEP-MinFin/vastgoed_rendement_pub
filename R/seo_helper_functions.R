#' SEO helper functions
#'
#' @param first.year First simulation year
#' @param n.year Number of simulation years
#' @param huisaantal Number of simulation houses
#' @param variantaantal Number of policy variants
#'
#' @return A vector with as many elements as houses has rows.
#' @import dplyr
#' @export

setup.data.frame_vec <- function(first.year, n.year,huisaantal,variantaantal) {
  years <- first.year:(first.year + n.year)
  homes <- 1:huisaantal
  variants <- 1:variantaantal
  combinations <- expand.grid(year = years,huis_id = homes,beleidsvariant = variants)
  df <- data.frame(combinations)

  df <- df %>%
    mutate(
      woz.woning = 0,
      oppervlak = 0,
      energie_label = 0,
      punten.wws = 0,
      punten.wws = 0,
      huur.wws = 0,
      huur.markt=0,
      huur.inkomsten = 0,
      jaarlijks.onderhoud = 0,
      investeringen.aanschaf = 0,
      investeringen.incidenteel = 0,
      investeringen.duurzaamheid = 0,
      verkoopwaarde = 0,
      pre.tax.net.cashflows = 0,
      post.tax.net.cashflows = 0,
      box3.percentage = 0
    )
  return(df)
}

###### wws.helper (args -> wws.rent) functie die wws huur gereneert als functie tijd. WOZ waarde zorgt voor tijdsafhankelijkheid
###### de functie geeft als output een list met twee componenten: de wws punten en de huur volgens het puntensysteem
###### let op: er wordt nu van uitgegaan dat er geen woning met een WOZ < 55.888 euro in de populatie zit
#' Title
#'
#' @param m2 -
#' @param energy.label bla
#' @param finish.level bla
#' @param woz bla
#' @param points.a1 bla
#' @param points.a4 bla
#' @param points.a56 bla
#' @param points.a9 bla
#' @param lookup.table bla
#' @param woz_index bla
#' @param type bla
#' @param bA1 bla
#' @param bA91 bla
#' @param bA91b bla
#'
#' @return bla
#' @export
wws.helper <- function(m2, energy.label, finish.level, woz, points.a1, points.a4, points.a56, points.a9, lookup.table, woz_index, type, bA1, bA91, bA91b){
  vA4 <- as.numeric(points.a4[points.a4['name'] == energy.label, c('value','delta')])
  if (delta_energie_label == 1){
    points.no.woz <- bA1*m2 + vA4[1] + vA4[2] # + vA56
  } else {
    points.no.woz <- bA1*m2 + vA4[1] # + vA56
  }
  if (type == "EGW"){
    points.no.woz <- points.no.woz + extra.points.egw
  }
  if (type == "MGW"){
    points.no.woz <- points.no.woz + extra.points.mgw
  }

  points.woz <- woz / (bA91*woz_index) + woz / m2 / (bA91b*woz_index) # parameters worden jaarlijks geindexeerd met gemiddelde woz stijging
  #print(points.no.woz + points.woz)
  #print(points.no.woz + points.woz)
  #print(points.woz/(points.no.woz + points.woz))
  if ((points.no.woz + points.woz) < bA93ii) {
    points <- points.no.woz + points.woz
  } else {
    if (points.woz/(points.no.woz + points.woz) > bA93i){
      capped.points.woz <- points.no.woz*bA93i/(1-bA93i)
      # print(cat("capped", "huis", h, "WWSpnt_cap", points.no.woz + capped.points.woz, "WWSpnt_no_cap", points.no.woz + points.woz))
    }
    if (points.woz/(points.no.woz + points.woz) <= bA93i){
      capped.points.woz <- points.woz
    }
    # # Dit klopte eerder niet. Hier stond eerst dat de WOZ maximaal 0,33 * non-WOZ punten kon geven, maar dat is niet zo.
    # # WOZ-punten mogen max 33% van het totaal zijn, dus max (33%/67%) van de non-WOZ punten.
    # max.points.woz <- points.no.woz*bA93i/(1-bA93i)
    # capped.points.woz <- min(max.points.woz, points.woz)
    points <- points.no.woz + capped.points.woz
  }
  print(points)
  wws.points <- floor(points)
  if (wws.points <= max(lookup.table$name)){
    wws.rent <- as.numeric(lookup.table[lookup.table['name'] == wws.points, 'value'])
  } else {
    wws.rent <- max(lookup.table$value)
  }

  results <- list(WOZ_punten = wws.points, WOZ_huur = wws.rent, WOZ_cap = points.woz/(points.no.woz + points.woz))
  return(results)
}

#




#calculate capped points if condition is met. Overwrite the woz_points
#' calc_capped_points
#'
#' @param X bla
#'
#' @export
calc_capped_points <- function(X){
  capped.points.woz <-
    ifelse(
      X$points_woz/(X$points_counter + X$points_woz) > bA93i & X$points_woz + X$points_counter > bA93ii,
      X$points_counter * bA93i/(1 - bA93i),
      capped.points.woz <- X$points_woz
    )
  points <- X$points_counter + capped.points.woz
  return(points)
}




#' base_points_calculate
#'
#' calculate base woz points
#'
#' @param X -
#'
#' @param value -
#' @param bA1 -
#'
#' @export
base_points_calculate<- function(X,value, bA1){
  point <-
    if_else(
      X$delta_energie_label == value ,
      bA1 * X$m2 + X$value,
      bA1 * X$m2 + X$value
      )
  return(point)
}


#' calculate_wws_points
#'
#' @param df_list_vec -
#'
#' @param delta_value -
#' @param bA1 -
#' @param bA91 -
#' @param bA91b -
#' @param lookup.table -
#'
#' @export
calculate_wws_points_old<-function(df_list_vec,delta_value, bA1, bA91, bA91b, lookup.table){
  df_list_vec['points_counter'] <- base_points_calculate(df_list_vec,delta_value, bA1)
  df_list_vec['points_counter'] <-
    df_list_vec$points_counter +
    if_else(df_list_vec$type == 'EGW', 31, 17)

  #Deze stap nog controleren DvdS - volgens mij moet deze meestijgen met index
  df_list_vec['points_woz'] <-
    df_list_vec$woz_indexed / (bA91 * df_list_vec$cumhpi) +
    df_list_vec$woz_indexed / df_list_vec$m2 / (bA91b * df_list_vec$cumhpi)

  df_list_vec['wws_points'] <-
    ifelse(
      (df_list_vec$points_counter + df_list_vec$points_woz) < bA93ii,
      df_list_vec$points_counter + df_list_vec$points_woz,
      calc_capped_points(df_list_vec))

  df_list_vec$wws_points <- round(df_list_vec$wws_points)
  return(df_list_vec)
}


#' calculate_wws_rent
#'
#' Get huur.wws based on wws.point
#'
#' @param houses -
#' @param lookup.table -

calculate_wws_rent <- function(houses,lookup.table){
  houses <- houses %>%
    left_join(
      lookup.table,
      by = c("wws_points"="name")
    ) %>%
    mutate(
      wws_rent = value_rent
    ) %>%
    tidyr::replace_na(list(wws_rent=max(lookup.table$value_rent))
    ) %>%
    mutate(
      wws_rent = 12 * wws_rent #* cumcpi
    )
}



#' read_huur_verhoging_scenario
#'
#' @param scenario_name -
#'
#' @return index
#' @export

read_huur_verhoging_scenario<-function(scenario_name){
  if(scenario_name=="vrijemarkt-SEO"){
    ## Maak een index die de maximale huurstijging weergeeft voor iemand die in de vrije sector huurt
    ## volgens beleidsparameters die beperkingen op huurstijgingen beschrijven.
    mgv <- cpi
    names(mgv) <- c("year","percentage")
    mgv[mgv["year"] == 2022,"percentage"] <- mgv_2022
    mgv[mgv["year"] == 2023,"percentage"] <- min(parameters[parameters["year"] == 2023,"cpi"],parameters[parameters["year"] == 2023,"loonstijging"]) + mgv_2023
    mgv[mgv["year"] == 2024,"percentage"] <- parameters[parameters["year"] == 2024,"loonstijging"] + mgv_2024
    mgv[mgv["year"] > 2024,"percentage"] <- parameters[parameters["year"] > 2024,"loonstijging"] + mgv_na_2024
    index<-mgv
  }
  else if(scenario_name=="sociaal_woco"){
    ##### Maak een index die de maximale huurstijging weergeeft voor iemand die in de sociale sector huurt
    ##### van een WOCO volgens beleidsparameters die beperkingen op huurstijgingen beschrijven.
    mgs <- cpi
    names(mgs) <- c("year","percentage")
    mgs[mgs["year"] == 2022,"percentage"] <- mgs_2022
    mgs[mgs["year"] == 2023,"percentage"] <- parameters[parameters["year"] == 2023,"loonstijging"] + mgs_2023
    mgs[mgs["year"] == 2024,"percentage"] <- parameters[parameters["year"] == 2024,"loonstijging"] + mgs_2024
    mgs[mgs["year"] > 2024,"percentage"] <- parameters[parameters["year"] > 2024,"loonstijging"] + mgs_na_2024
    index <-  mgs
  }
  else if (scenario_name=="sociaal_particulier"){
    ##### Maak een index die de maximale huurstijging weergeeft voor iemand die in de sociale sector huurt
    ##### van een particulier volgens beleidsparameters die beperkingen op huurstijgingen beschrijven.
    mgsp <- cpi
    names(mgsp) <- c("year","percentage")
    mgsp[mgsp["year"] == 2022,"percentage"] <- mgs_2022_part # hier verschil tov woco waar 2.3% staat - hier 3.3%
    mgsp[mgsp["year"] == 2023,"percentage"] <- parameters[parameters["year"] == 2023,"loonstijging"] # hier verschil tov woco waar  + mgs_2023 = -0.5% staat
    mgsp[mgsp["year"] == 2024,"percentage"] <- parameters[parameters["year"] == 2024,"loonstijging"] # hier verschil tov woco waar  + mgs_2024 = -0.5% staat
    mgsp[mgsp["year"] > 2024,"percentage"] <- parameters[parameters["year"] > 2024,"loonstijging"] # hier verschil tov woco waar  + mgs_na_2024 = -0.5% staat
    index <- mgsp
  }
  else if (scenario_name=="sociaal_oud"){
    mgs_oudv <- cpi
    names(mgs_oudv) <- c("year","percentage")
    mgs_oudv[mgs_oudv["year"] == 2022,"percentage"] <- mgs_2022
    mgs_oudv[mgs_oudv["year"] >= 2023,"percentage"] <- parameters[parameters["year"] >= 2023,"cpi"] + mgs_oud # nog kiezen: CPI of loonstijging
    index <-  mgs_oudv
  }
  else if (scenario_name=="middenhuur"){
    ##### Maak een index die de maximale huurstijging weergeeft voor iemand die in de midden sector huurt
    ##### volgens beleidsparameters die beperkingen op huurstijgingen beschrijven.
    mgm <- cpi
    names(mgm) <- c("year","percentage")
    mgm[mgm["year"] == 2022,"percentage"] <- mgm_2022
    mgm[mgm["year"] == 2023,"percentage"] <- min(parameters[parameters["year"] == 2023,"cpi"],parameters[parameters["year"] == 2023,"loonstijging"]) + mgv_2023
    mgm[mgm["year"] == 2024,"percentage"] <- parameters[parameters["year"] == 2024,"loonstijging"] + mgm_2024
    mgm[mgm["year"] > 2024,"percentage"] <- parameters[parameters["year"] > 2024,"loonstijging"] + mgm_na_2024
    index <- mgm
  }

  return (index)
}


#' calc_huurstijging
#'
#' bla
#'
#' @param scenario_name bla
#' @param df_list_vec bla
#'
#' @return bla
#' @export
calc_huurstijging<-function(scenario_name,df_list_vec){
  index<-read_huur_verhoging_scenario("vrijemarkt-SEO")
  #Add other steps later on
}

# # wws.helper_vec <- function(m2, energy.label, finish.level, woz, woz_index, type, value, delta,delta_energie_label){
#
#
#   points.woz <- woz / (bA91*woz_index) + woz / m2 / (bA91b*woz_index) # parameters worden jaarlijks geindexeerd met gemiddelde woz stijging
#   ifelse((points.no.woz + points.woz) < bA93ii,
#     points <<- points.no.woz + points.woz,
#   ifelse(points.woz/(points.no.woz + points.woz) > bA93i,
#       capped.points.woz <<- points.no.woz*bA93i/(1-bA93i),
#       # print(cat("capped", "huis", h, "WWSpnt_cap", points.no.woz + capped.points.woz, "WWSpnt_no_cap", points.no.woz + points.woz))
#     ifelse(points.woz/(points.no.woz + points.woz) <= bA93i,
#       capped.points.woz <<- points.woz,
#     # # Dit klopte eerder niet. Hier stond eerst dat de WOZ maximaal 0,33 * non-WOZ punten kon geven, maar dat is niet zo.
#     # # WOZ-punten mogen max 33% van het totaal zijn, dus max (33%/67%) van de non-WOZ punten.
#     # max.points.woz <- points.no.woz*bA93i/(1-bA93i)
#     # capped.points.woz <- min(max.points.woz, points.woz)
#     points <<- points.no.woz + capped.points.woz)))
#
#   wws.points <<- floor(points)
#   wws.rent<<-ifelse(wws.points <= max(lookup.table$name),
#     as.numeric(lookup.table[lookup.table['name'] == wws.points, 'value']),
#     max(lookup.table$value)
#   )
#
#   results <- list(WOZ_punten = wws.points, WOZ_huur = wws.rent, WOZ_cap = points.woz/(points.no.woz + points.woz))
#   return(results)
# }


#' verhoging
#'
#' @param huur_tmin1 -
#' @param pntgrens.sociaal -
#' @param pntgrens.midden -
#' @param wws.punten -
#' @param max.groei.sociaal -
#' @param max.groei.sociaal.part -
#' @param max.groei.midden -
#' @param max.groei.vrij -
#' @param huur.wws -
#' @param vrije.huur -
#' @param jaar -
#' @param type -
#' @param opslag_middenhuur -
#' @param huur_oud -
#' @param max.groei.sociaal.oud -
#' @param i bla
#'
#' @return bla
#' @export

verhoging <- function(huur_tmin1, pntgrens.sociaal, pntgrens.midden, wws.punten, max.groei.sociaal, max.groei.sociaal.part,
                      max.groei.midden, max.groei.vrij, huur.wws, vrije.huur, jaar, type, opslag_middenhuur, huur_oud, max.groei.sociaal.oud, i){

  # huur_tmin1 - de huur op t-1
  # pntgrens.sociaal - de puntengrens sociale huur
  # pntgrens.midden - de puntengrens midden huur
  # wws.punten - matrix[jaar, wws punten] voor huis
  # max.groei.sociaal - matrix[jaar, max groei percentage sociale huur woco]
  # max.groei.sociaal.part - matrix[jaar, max groei percentage sociale huur part]
  # max.groei.midden - matrix[jaar, max groei percentage sociale huur midden]
  # max.groei.vrij - matrix[jaar, max groei percentage sociale huur vrij]
  # huur.wws - matrix[jaar, max huur volgens puntensysteem]
  # huur.markt - matrix[jaar, max markthuur volgens handboek]
  # jaar - jaar waarin huur bepaald moet worden
  # type - type investeerder

  wws.pnt <- as.numeric(wws.punten[wws.punten['year'] == jaar, 'punten']) # wat is het aantal wws punten in jaar
  wws <- as.numeric(huur.wws[huur.wws['year'] == jaar, 'huur']) # wat is de maximale wws huur in jaar
  vrije.markt <- as.numeric(vrije.huur[vrije.huur['year'] == jaar, 'huur']) # wat is de maximale markt huur in jaar
  if ((wws.pnt < pntgrens.sociaal) & (type == "part_box2" | type == "part_box3") & huur_oud == "nee"){
    groei <- as.numeric(max.groei.sociaal.part[max.groei.sociaal.part['year'] == jaar, 'percentage'])
    huur <- min((1 + groei)*huur_tmin1, wws)
  }
  if ((wws.pnt < pntgrens.sociaal) & (type == "inst_vpb" | type == "woco") & huur_oud == "nee"){
    groei <- as.numeric(max.groei.sociaal[max.groei.sociaal['year'] == jaar, 'percentage'])
    huur <- min((1 + groei)*huur_tmin1, wws)
  }
  if ((wws.pnt < pntgrens.sociaal) & (type == "part_box2" | type == "part_box3") & huur_oud == "ja"){
    groei <- as.numeric(max.groei.sociaal.oud[max.groei.sociaal.oud['year'] == jaar, 'percentage'])
    huur <- min((1 + groei)*huur_tmin1, wws)
  }
  if ((wws.pnt < pntgrens.sociaal) & (type == "inst_vpb" | type == "woco") & huur_oud == "ja"){
    groei <- as.numeric(max.groei.sociaal.oud[max.groei.sociaal.oud['year'] == jaar, 'percentage'])
    huur <- min((1 + groei)*huur_tmin1, wws)
  }
  if (pntgrens.sociaal <= wws.pnt & wws.pnt <= pntgrens.midden & ((jaar == 2023) | i == 1)){
    groei <- as.numeric(max.groei.midden[max.groei.midden['year'] == jaar, 'percentage'])
    huur <- min((1 + groei)*huur_tmin1,  vrije.markt)
  }
  if (pntgrens.sociaal <= wws.pnt & wws.pnt <= pntgrens.midden & (jaar > 2023) & i > 1){
    groei <- as.numeric(max.groei.midden[max.groei.midden['year'] == jaar, 'percentage'])
    huur <- min((1 + groei)*huur_tmin1, wws*(1+opslag_middenhuur))
  }
  if (pntgrens.midden < wws.pnt){
    groei <- as.numeric(max.groei.vrij[max.groei.vrij['year'] == jaar, 'percentage'])
    huur <- min((1 + groei)*huur_tmin1, vrije.markt)
  }
  return(huur)
}


#' calc.irr
#'
#' bla
#'
#' @param df dataframe
#' @param colname bla
#'
#' @return bla
#' @export
calc.irr <- function(df, colname) {
  cf <- as.vector(unlist(df[colname]))
  sol <- irr(cf)
  return(sol)
}



# LWR tabel loopt op in waarden, dus we kunnen vanaf het begin er doorheen lopen en gelijk stoppen als we een match tegenkomen (via de return() functie)
#' call.leegwaarde
#'
#' @param huur -
#'
#' @param WOZ -
#'
#' @export
call.leegwaarde <- function(huur, WOZ){
  ratio_huur_WOZ <- huur / WOZ
  if (lwr=="nieuw"){
    for(index in 1:nrow(lwrtabel)){
      if(ratio_huur_WOZ < lwrtabel$bovengrens[index]){
        return(as.numeric(lwrtabel[index, 'leegwaarderatio']) * WOZ)
      }
    }
  }
  if (lwr=="oud"){
    for(index in 1:nrow(lwrtabel_oud)){
      if(ratio_huur_WOZ < lwrtabel_oud$bovengrens[index]){
        return(as.numeric(lwrtabel_oud[index, 'leegwaarderatio']) * WOZ)
      }
    }
  }
  # Als ze op de een of andere manier boven 100% uit komen, dan geven we maar gewoon de max-waarde terug. Zou wel een bijzonder huis zijn als de huur boven de WOZ ligt.
  return(as.numeric(lwrtabel$leegwaardenratio[nrow(lwrtabel)]) * WOZ)
}

#' calc.verhuurdersheffing
#'
#' @param df -
#'
#' @param percentage -
#' @param aftopping -
#'
#' @export
calc.verhuurdersheffing <- function(df,percentage,aftopping){
  belasting <- percentage*pmin(df$woz_value,aftopping)
  return(belasting)
}

#' plot_huur_list
#'
#' @param house_num -
#'
#' @param var_list -
#'
#' @export
plot_huur_list <- function(house_num, var_list){
  eval_string <- paste0("ggplot(data=df_list[[", house_num, "]], aes(x=year, y=", var_list[1], "))  + geom_line(aes(y = ", var_list[1], "))")
  if(length(var_list) > 1){
    for(item in 2:length(var_list)){
      eval_string <- paste0(eval_string, " + geom_line(aes(y = ", var_list[item], "))")
    }
  }
  print(eval_string)
  eval(eval_string)
  return(0)
}

#' print_init_wws
#'
#' @param var bla
#'
#' @return bla
#' @export
print_init_wws <- function(var){
  for(index in 1:length(df_list)){
    print(df_list[[index]][1,var])
  }
}

# box3 onder oude belastingregime (voor 2023) berekenen
#' call.box3_oud
#'
#' @param vermogen -
#' @param heffingsvrij -
#' @param schijf1 -
#' @param schijf2 -
#' @param forf1 -
#' @param forf2 -
#' @param forf3 -
#' @param perc3 -
#'
#' @export
call.box3_oud <- function(vermogen, heffingsvrij, schijf1, schijf2, forf1, forf2, forf3, perc3){
  belasting <- case_when(vermogen <= heffingsvrij ~ 0,
                         vermogen > heffingsvrij & vermogen <= heffingsvrij+schijf1 ~ perc3*forf1*(vermogen-heffingsvrij),
                         vermogen > heffingsvrij+schijf1 & vermogen <= heffingsvrij+schijf2 ~ perc3*forf1*schijf1+perc3*forf2*(vermogen-heffingsvrij-schijf1),
                         vermogen > heffingsvrij+schijf2 ~ perc3*forf1*schijf1 + perc3*forf2*(schijf2-schijf1) + perc3*forf3*(vermogen-heffingsvrij-schijf2))
  return(belasting)
}

#' call.plot.huis
#'
#' @param df bla
#' @param huisnr bla
#' @param vars bla
#'
#' @return bla
#' @export
call.plot.huis <- function(df,huisnr,vars){
  hulp <- as.data.frame(df[[huisnr]])
  hulp <- hulp[,c("year",vars)]
  hulp <- hulp %>% pivot_longer(cols=vars,
                                names_to='jaar',
                                values_to='inkomsten')
  ggplot(hulp, aes(x=year, inkomsten, color=jaar)) + geom_line()
}

# functies voor verduurzamen
#' label_lookup
#'
#' @param label -
#'
#' @export
label_lookup <- function(label){
  return(as.numeric(label_nums[label_nums['label_list'] == label, 'label_vals']))
}

#' verduurzamen
#'
#' @param m2 -
#'
#' @param start_label -
#' @param label_target -
#'
#' @export
verduurzamen <- function(m2, start_label, label_target){
  oud_label <- substring(start_label, 5)
  num_oud_label <- label_lookup(oud_label)
  num_target_label <- label_lookup(label_target) + 1 # In de duurzaamheid_matrix tabel zijn de kolommen met 1 verschoven, omdat de eerste kolom ook labels bevat.
  kosten_per_m2 <- as.numeric(duurzaamheid_matrix [num_oud_label, num_target_label])
  totale_kosten <- 0
  if (num_target_label < num_oud_label){
    totale_kosten <- kosten_per_m2 * m2 + verduurzaming_starttarief
  }
  return(totale_kosten)
}

#' call.eindwaarde.doorexploiteren
#'
#' bla
#'
#' @param CF bla
#' @param d bla
#' @param g bla
#' @param m bla
#'
#' @return bla
#' @export
call.eindwaarde.doorexploiteren <- function(CF,d,g,m){
  # input zijn een vector van cashflows en vectoren van discountwaarde d, groeivoet g en mutatiegraad m
  # de functie laat alleen de (1+d)^0.5 weg (het handboek disconteerd halverwege het jaar
  EW <- CF*(1-m)*(1+g)/(d-((1-m)*(1+g)-1))
  return(EW)
}

