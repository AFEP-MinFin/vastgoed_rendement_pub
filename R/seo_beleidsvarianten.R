#' SEO beleidsvarianten vector
#'
#' bla
#'
#' @param beleid Beleid df (read in pipeline)
#' @param variant_num The policy variant index
#'
#' @return bla
#' @export
get_beleidsparameters <- function(beleid, variant_num){
  # De "assign" functies hieronder zorgen ervoor dat de waardes niet alleen lokaal binnen de functie beschikbaar zijn, maar ook globaal.
  # Als je het alleen in de 'parent' wilt hebben, kun je ook de operator <<- gebruiken.
  # Dit is normaal eigenlijk een teken van slechte code, maar wel het makkelijkste om compatibility te behouden met het bestaande script.

  # WWS puntengrenzen
  #    assign("grens.sociaal", as.numeric(beleid[beleid['beleid'] == 'sociale_huurgrens', 'parameter']), envir = .GlobalEnv)
  #    assign("grens.midden", as.numeric(beleid[beleid['beleid'] == 'midden_huurgrens', 'parameter']), envir = .GlobalEnv)
  #    assign("huurgrens_sociaal", grens.sociaal * cpi_hpi_dv$cumcpi, envir= .GlobalEnv) # PM Nog een beetje onduidelijk qua naamgeving deze
  #    assign("huurgrens_midden", grens.midden * cpi_hpi_dv$cumcpi, envir=.GlobalEnv) # PM Nog een beetje onduidelijk qua naamgeving deze
  assign("puntengrens_sociaal", as.numeric(beleid[beleid['beleid'] == 'sociale_huur_punten', variant_num]), envir = .GlobalEnv)
  assign("puntengrens_midden", as.numeric(beleid[beleid['beleid'] == 'midden_huur_punten', variant_num]), envir = .GlobalEnv)

  # maximale stijgingspercentages vrije sector
  assign("mgv_2022", as.numeric(beleid[beleid['beleid'] == 'max_huurv_vrij_2022', variant_num]), envir = .GlobalEnv)
  assign("mgv_2023", as.numeric(beleid[beleid['beleid'] == 'max_huurv_vrij_2023', variant_num]), envir = .GlobalEnv)
  assign("mgv_2024", as.numeric(beleid[beleid['beleid'] == 'max_huurv_vrij_2024', variant_num]), envir = .GlobalEnv)
  assign("mgv_na_2024", as.numeric(beleid[beleid['beleid'] == 'max_huurv_vrij_na_2024', variant_num]), envir = .GlobalEnv)

  # maximale stijgingspercentages sociale huur woco en particulier
  assign("mgs_2022", as.numeric(beleid[beleid['beleid'] == 'max_huurv_soc_2022', variant_num]), envir = .GlobalEnv)
  assign("mgs_2022_part", as.numeric(beleid[beleid['beleid'] == 'max_huurv_soc_part_2022', variant_num]), envir = .GlobalEnv)
  assign("mgs_2023", as.numeric(beleid[beleid['beleid'] == 'max_huurv_soc_2023', variant_num]), envir = .GlobalEnv)
  assign("mgs_2024", as.numeric(beleid[beleid['beleid'] == 'max_huurv_soc_2024', variant_num]), envir = .GlobalEnv)
  assign("mgs_na_2024", as.numeric(beleid[beleid['beleid'] == 'max_huurv_soc_na_2024', variant_num]), envir = .GlobalEnv)

  # maximale huurstijging in 'oude' huurstelsel --> nu gezet op cpi + 1%
  assign("mgs_oud", as.numeric(beleid[beleid['beleid'] == 'max_huurv_soc_oud', variant_num]), envir = .GlobalEnv)
  assign("huur_oud", as.character(beleid[beleid['beleid'] == 'huur_oud', variant_num]), envir = .GlobalEnv)

  # maximale stijgingspercentages midden huur
  assign("mgm_2022", as.numeric(beleid[beleid['beleid'] == 'max_huurv_mid_2022', variant_num]), envir = .GlobalEnv)
  assign("mgm_2023",  as.numeric(beleid[beleid['beleid'] == 'max_huurv_mid_2023', variant_num]), envir = .GlobalEnv)
  assign("mgm_2024", as.numeric(beleid[beleid['beleid'] == 'max_huurv_mid_2024', variant_num]), envir = .GlobalEnv)
  assign("mgm_na_2024", as.numeric(beleid[beleid['beleid'] == 'max_huurv_mid_na_2024', variant_num]), envir = .GlobalEnv)

  # WWS maximum percentage dat WOZ mee mag tellen
  assign("bA93i", 0.33, envir = .GlobalEnv)
  assign("bA93ii",as.numeric(beleid[beleid['beleid'] == 'wws_woz_grens', variant_num]), envir = .GlobalEnv)

  # Box 2
  assign("box2_hoog", as.numeric(beleid[beleid['beleid'] == 'box2_hoog', variant_num]), envir = .GlobalEnv)
  assign("box2_schijfgrens", as.numeric(beleid[beleid['beleid'] == 'box2_schijfgrens', variant_num]), envir = .GlobalEnv)

  # Box 3
  assign("perc.box3", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_perc', variant_num]), envir = .GlobalEnv)
  assign("forf.bezit.box3", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_bezit', variant_num]), envir = .GlobalEnv)
  assign("forf.schuld.box3", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_schuld', variant_num]), envir = .GlobalEnv)
  assign("heffingsvrij.verm", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_vrijstelling', variant_num]), envir = .GlobalEnv)
  assign("opslag_middenhuur", as.numeric(beleid[beleid['beleid'] == 'opslag_middenhuur', variant_num]), envir = .GlobalEnv)

  #Box 3 percentages
  assign("perc.box3.2022", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_2022', variant_num]), envir = .GlobalEnv)
  assign("perc.box3.2023", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_2023', variant_num]), envir = .GlobalEnv)
  assign("perc.box3.2024", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_2024', variant_num]), envir = .GlobalEnv)
  assign("perc.box3.2025", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_2025', variant_num]), envir = .GlobalEnv)

  # Box 3 oude versie (voor 2023)
  assign("belasting_box3_perc_oud", as.numeric(beleid[beleid['beleid'] == 'belasting_box3_perc_oud', variant_num]), envir = .GlobalEnv)
  assign("heffingsvrij_box3_oud", as.numeric(beleid[beleid['beleid'] == 'hefingsvrij_box3_oud', variant_num]), envir = .GlobalEnv)
  assign("schijf1_box3_oud", as.numeric(beleid[beleid['beleid'] == 'schijf1_box3_oud', variant_num]), envir = .GlobalEnv)
  assign("schijf2_box3_oud", as.numeric(beleid[beleid['beleid'] == 'schijf2_box3_oud', variant_num]), envir = .GlobalEnv)
  assign("forfait_schijf1_oud", as.numeric(beleid[beleid['beleid'] == 'forfait_schijf1_oud', variant_num]), envir = .GlobalEnv)
  assign("forfait_schijf2_oud", as.numeric(beleid[beleid['beleid'] == 'forfait_schijf2_oud', variant_num]), envir = .GlobalEnv)
  assign("forfait_schijf3_oud", as.numeric(beleid[beleid['beleid'] == 'forfait_schijf3_oud', variant_num]), envir = .GlobalEnv)
  assign("box3_oud", as.character(beleid[beleid['beleid'] == 'box3_oud', variant_num]), envir = .GlobalEnv)

  # VPB grenzen
  assign("box2_laag", as.numeric(beleid[beleid['beleid'] == 'box2_laag', variant_num]), envir = .GlobalEnv)
  assign("vpb_laag", as.numeric(beleid[beleid['beleid'] == 'vpb_laag', variant_num]), envir = .GlobalEnv)
  assign("vpb_hoog", as.numeric(beleid[beleid['beleid'] == 'vpb_hoog', variant_num]), envir = .GlobalEnv)
  assign("gem_IB_pensioengerechtigde", as.numeric(beleid[beleid['beleid'] == 'gem_IB_pensioengerechtigde', variant_num]), envir = .GlobalEnv)

  # Verhuurdersheffing
  assign("verhuurdersheffing", as.numeric(beleid[beleid['beleid'] == 'verhuurdersheffing_perc', variant_num]), envir = .GlobalEnv)
  assign("verhuurdersheffing.grens", as.numeric(beleid[beleid['beleid'] == 'verhuurdersheffing_grens', variant_num]), envir = .GlobalEnv)

  # Overdrachtsbelasting percentage
  assign("overdrachtsbelasting.perc", as.numeric(beleid[beleid['beleid'] == 'overdrachtsbelasting', variant_num]), envir = .GlobalEnv)

  # Verduurzaming
  assign("label_target", as.character(beleid[beleid['beleid'] == 'doel_energie_label', variant_num]), envir=.GlobalEnv)
  assign("jaar_verduurzaming", as.numeric(beleid[beleid['beleid'] == 'jaar_energie_label', variant_num]), envir=.GlobalEnv)
  assign("delta_energie_label", as.numeric(beleid[beleid['beleid'] == 'delta_energie_label', variant_num]), envir=.GlobalEnv)

  # vrij huur
  assign("markt_huur_perc", as.numeric(beleid[beleid['beleid'] == 'markt_huur_perc', variant_num]), envir=.GlobalEnv)

  # leegwaarderatio
  assign("lwr", as.character(beleid[beleid['beleid'] == 'lwr', variant_num]), envir=.GlobalEnv)

}
