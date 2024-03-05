# Harvester time consumption model ----
#' t_harv_bledning_joh15
#'
#' @description
#' `t_harv_bledn_joh15()` estimate harvester time consumption in forest operations, according
#' to models presented "bledningshogst" by Jonsson in 2015
#'
#' @details
#'
#'
#' @param stands_df A data frame having stand characteristics to be used in the
#' function. Must include:
#' \itemize{
#' \item 'Stems_ha' Number of stems per hectare,
#' \item 'v' mean volume m3fub of the (harvestable) trees in the stand.
#' }
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
#' t_harv_bledn_joh15(stands_df = testdata_heureka_clearcutting, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_bledn_joh15(stands_df = testdata_heureka_thinning, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_bledn_joh15(stands_df = testdata_sensblt_v_Y_L_D, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_bledn_joh15(stands_df = testdata_clearcut_forw, verbose = TRUE) %>% dplyr::glimpse()
t_harv_bledn_joh15 = function(stands_df, treatmentnumber = 2,
                     verbose = FALSE){


  stopifnot(is.data.frame(stands_df))
  stopifnot(c("v", "Stems_ha") %in% names(stands_df))

  # prepping data frame ----
  # If variables are specified in df; keep them, if not; use as defined in function
  standvars <- colnames(stands_df)
  if(!('treatmentnumber' %in% standvars)) { stands_df$treatmentnumber <- treatmentnumber}

stands_df <- stands_df %>% dplyr::mutate(
    V.init = .data$v * .data$Stems_ha
    , modelversion = "Johansson2015"
    , treatmentnumber = treatmentnumber

  ) %>% dplyr::mutate( # nharv + Vhav are pure guestimates
    Nharv = dplyr::case_when(treatmentnumber == 1 ~ .data$Stems_ha / 3.5,
                      treatmentnumber != 1 ~ .data$Stems_ha / 5)
    , Vharv = dplyr::case_when(treatmentnumber == 1 ~ .data$V.init / 3,
                        treatmentnumber != 1 ~ .data$V.init / 4),
    v_hrv = .data$Vharv / .data$Nharv # v_hrv is the average volume of harvested stems

  )

  #  t1 Harvester driving time seems to be not part of the model

stands_df <- stands_df %>% dplyr::mutate(t1 = 0)

  # t2 Felling and processing time, cmin per tree ----

stands_df <- stands_df %>%
  dplyr::mutate(
    t2 = dplyr::case_when(.data$treatmentnumber == 1 ~ 48 + 211 * .data$v_hrv,
                        .data$treatmentnumber == 2 ~ 63 + 275 * .data$v_hrv,
                        TRUE ~ 63 + 275 * .data$v_hrv),
    treatment = dplyr::case_when(.data$treatmentnumber == 1 ~ "Innledende bledning",
                                 .data$treatmentnumber == 2 ~ "Senere bledning",
                                 TRUE ~ "Senere bledning")
  )

# t3 Additional harvesting time, cmin per tree seems to not to be part of the model ----
  stands_df <- stands_df %>%
  dplyr::mutate( t3 = 0)

  # wrap up ----

    stands_df <- stands_df  %>%
      dplyr::mutate(ttot_cmin_tree = .data$t1 + .data$t2 + .data$t3) %>%
      dplyr::mutate(
                  harv_G15min.tree = .data$ttot_cmin_tree * 1.3 / 100,
                  harv_G15min.ha = .data$harv_G15min.tree * .data$Nharv,
                  harv_G15h.ha = round(.data$harv_G15min.ha / 60, 3),
                  harv_G15min.m3 = round(.data$harv_G15min.ha / .data$Vharv, 3),
                  harv_m3.G15h = round(60 / .data$harv_G15min.m3, 3),
                  harv_stems.G15h = round(60/.data$harv_G15min.tree, 1)) %>%
    dplyr::select( tidyselect::starts_with("harv_"), tidyselect::everything())


  if(verbose == FALSE){
    stands_df <- stands_df %>%
      dplyr::select(tidyselect::all_of(standvars),
                    tidyselect::starts_with("harv_G"), "harv_m3.G15h", "treatment", "modelversion", "treatmentnumber")
  }
  return(stands_df)
}
