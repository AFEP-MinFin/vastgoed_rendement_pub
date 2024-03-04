#' Get max rent increase index, based on a specific scenario that corresponds with
#' #'
#' @param scenario_name string of a scenario that corresponds with an
#' @return A IRR value
#' @export
construct_indices<-function(scenario_name){
  if(scenario_name=="vrijemarkt"){
    ## Maak een index die de maximale huurstijging weergeeft voor iemand die in de vrije sector huurt
    ## volgens beleidsparameters die beperkingen op huurstijgingen beschrijven.
    mgv <- cpi
    names(mgv) <- c("year","percentage")
    mgv[mgv["year"] == 2022,"percentage"] <- mgv_2022
    mgv[mgv["year"] == 2023,"percentage"] <- mgv_2023 #+ min(parameters[parameters["year"] == 2023,"cpi"],parameters[parameters["year"] == 2023,"loonstijging"])
    mgv[mgv["year"] == 2024,"percentage"] <- mgv_2024 #+parameters[parameters["year"] == 2024,"loonstijging"]
    mgv[mgv["year"] > 2024,"percentage"] <- mgv_na_2024 #+ parameters[parameters["year"] > 2024,"loonstijging"]
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
