# Forwarder time consumption model for bledningshogst ----
#' t_forw_bledn_joh15
#'
#' @description
#' `t_forv_bledn_joh15()` estimate harvester time consumption in forest operations, according
#' to models presented in the swedish Heureka description,
#' and a later modified version by Bruce Talbod (NIBIO Norway), and a
#' model for "bledningshogst" by Jonsson in 2015

#'
#' @details
#' variables to provide is listed under heading "3 Variables" in the Heureka
#' time consumption functions description
#' https://www.heurekaslu.se/w/images/8/8c/Time_consumption_harvester_and_forwarder.pdf
#' Clearcut harvester model is the Heureka clearcut model, being a
#' slightly modified version of Brunberg 1995.
#' Thinning harvester model is the Heureka clearcut model, being a
#' slightly modified version of Brunberg1997
#'
#'
#' @param stands_df A data frame having stand characteristics to be used in the
#' function. Must include:
#' 'Stems_ha' Number of stems per hectare,
#' 'v' mean volume m3fub of the harvested trees in the stand.
#' Can also include:
#' Nharv number of harvested stems per hectare
#' 'p_init_spruce' proportion of spruce before harvest
#' 'L' slope class (1:5) where  1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\%
#' 'Y' surface structure (1:5) where 5 is uneven
#'

#' @param treatmentnumber one of: 1, 2. 1 is first (initiating bledning from even-aged stand), 2 is later bledning where striproads exist
#' @param verbose if TRUE: returning all time components from time consumption models
#'
#' @returns A [tibble()].
#' If verbose TRUE; estimates from sub calculations are also present in the table.
#' If verbose FALSE; time consumption per production unit and productivity in
#'   the following variables:
#'  harv_G15min.tree (harvest time, G15 minutes per tree)
#'  harv_G15h.ha (harvest time, G15 hours per hectare)
#' harv_G15min.m3 (harvest time, G15 minutes per m3)
#' harv_m3.G15h (productivity, m3 per G15-hour)
#'
#' @export
#'
#' @examples
#' t_forw_bledn_joh15(stands_df = testdata_heureka_clearcutting, verbose = TRUE) %>% dplyr::glimpse()
#' t_forw_bledn_joh15(stands_df = testdata_heureka_thinning, verbose = TRUE) %>% dplyr::glimpse()
#' t_forw_bledn_joh15(stands_df = testdata_sensblt_v_Y_L_D, verbose = TRUE) %>% dplyr::glimpse()
#' t_forw_bledn_joh15(stands_df = testdata_fjeld, verbose = TRUE) %>% dplyr::glimpse()
t_forw_bledn_joh15 = function(stands_df, treatmentnumber = 2,
                     verbose = FALSE){


  stopifnot(is.data.frame(stands_df))
  stopifnot(c("v", "Stems_ha") %in% names(stands_df))

  # prepping data frame ----
  # If variables are specified in df; keep them, if not; use as defined in function
  standvars <- colnames(stands_df)
  if(!('treatmentnumber' %in% standvars)) { stands_df$treatmentnumber <- treatmentnumber}

stands_df <- stands_df %>% dplyr::mutate(
    V.init = .data$v * .data$Stems_ha
    , modelversion = "Johansson 2015 bledn forw"
    ) %>%
  dplyr::mutate( # nharv + Vhav are pure guestimates
    Nharv = dplyr::case_when(treatmentnumber == 1 ~ .data$Stems_ha / 3.5,
                           treatmentnumber != 1 ~ .data$Stems_ha / 5)
  , Vharv = dplyr::case_when(treatmentnumber == 1 ~ .data$V.init / 3,
                             treatmentnumber != 1 ~ .data$V.init / 4),
  v_hrv = .data$Vharv / .data$Nharv # v_hrv is the average volume of harvested stems

)


t_forw <- Fomir::t_forw_bb(stands_df, forw_size = "medium", modelversion = c("Brunberg04"),
                      treatment = c("thinning"), harvest_strength_pct = 30, #pct basal area
                      verbose = TRUE)


stands_df <- t_forw



  if(verbose == FALSE){
    stands_df <- stands_df %>% dplyr::select(tidyselect::all_of(standvars), tidyselect::starts_with("harv_G"), "harv_m3.G15h", "treatment", "modelversion", "treatmentnumber")
  }
  return(stands_df)
}
