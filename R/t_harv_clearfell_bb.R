


# Harvester time consumption model ----
#' t_harv_clearfell_bb
#'
#' @description
#' `t_harv_clearfell_bb()` estimate harvester time consumption in forest operations, according
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
#' function.
#' May include all the parameters set in the function call except "verbose"
#' @param Stems_ha Number of stems per hectare,
#' @param v mean volume m3fub of the trees in the stand.
#' @param L slope class (1:5) where  1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\%
#' @param Y surface structure (1:5) where 5 is uneven
#' @param modelversion one of "Brunberg95" or "Talbot16"
#' @param verbose if TRUE: returning all time components from time consumption models
#'
#' @returns A tibble.
#' If verbose TRUE; estimates from sub calculations are also present in the table.
#' If verbose FALSE; time consumption per production unit and productivity in
#'   the following variables:
#' \itemize{
#' \item harv_G15min.tree (harvest time, G15 minutes per tree)
#' \item harv_G15h.ha (harvest time, G15 hours per hectare)
#' \item  harv_G15min.m3 (harvest time, G15 minutes per m3)
#' \item  harv_m3.G15h (productivity, m3 per G15-hour)
#' }
#'
#' @export
#'
#' @examples
#' t_harv_clearfell_bb(stands_df = testdata_heureka_clearcutting,  verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_clearfell_bb(stands_df = testdata_heureka_clearcutting, modelversion = "Talbot16", verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_clearfell_bb(stands_df = testdata_heureka_clearcutting, modelversion = "Brunberg07", verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_clearfell_bb(stands_df = testdata_heureka_thinning, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_clearfell_bb(stands_df = testdata_clearcut_forw, modelversion = "Talbot16",  verbose = TRUE) %>% dplyr::glimpse()
t_harv_clearfell_bb = function(stands_df = NULL, Stems_ha = 1000, v = 0.5, L = 2, Y = 2,
                     modelversion = c("Brunberg95", "Talbot16", "Brunberg07")[1],
                      verbose = FALSE){

  if(is.null(stands_df)){
    stands_df <- data.frame(Stems_ha = Stems_ha, v = v)
  }
  stopifnot(is.data.frame(stands_df))

  # prepping data frame ----
  # If variables are specified in df; keep them, if not; use as defined in function
  standvars <- colnames(stands_df)
  stopifnot(c("v", "Stems_ha") %in% standvars)

  if(!('L' %in% standvars)) { stands_df$L <- L}
  if(!('Y' %in% standvars)) { stands_df$Y <- Y}
  if(!('modelversion' %in% standvars)) { stands_df$modelversion = modelversion}
  if(!('Nharv' %in% standvars)) { stands_df$Nharv = stands_df$Stems_ha}


  stands_df <- stands_df %>% dplyr::mutate(
    Nres = .data$Stems_ha - .data$Nharv
    , V.init = .data$Stems_ha * .data$v
    , v_hrv =  .data$v
    , Vharv = .data$v_hrv * .data$Nharv
    , v = trunk_rng(.data$v, 0, 3)
    , L = trunk_rng(.data$L, 1, 4)
    , Y = trunk_rng(.data$Y, 1, 4)
    , treatment = "clearcutting"
  )


  # heureka t1 Harvester driving time in clearcutting and thinning, cmin / tree ----

    stands_df <- stands_df %>%
    dplyr::mutate(
                  Nharv_tr =  trunk_rng(.data$Nharv, 200, 1500),
                  K = dplyr::case_when(
                    .data$treatment == "clearcutting" & .data$modelversion == "Brunberg95" ~ 25.9, #Heureka doc v1.3 p5
                    .data$treatment == "clearcutting" & .data$modelversion == "Brunberg07" ~ 31, # Brunberg07 p6
                                TRUE  ~ 25.9),
                  S = 13.3,
                  t1 = 10^6 / (.data$S * .data$Nharv_tr * .data$K * (1 + (50/.data$Nharv_tr) - 0.1*.data$Y - 0.1*.data$L)), # cmin/tree. Source: Heureka doc v1.3p and Brunberg 2007 p5.
                  t1 = trunk_rng(.data$t1, 2, 20)
                  )


  # heureka t2 Felling and processing time, cmin per tree ----
    stands_df <-  stands_df %>%
      dplyr::mutate(
        phindrance = 0.35/ (1 + exp(2.5*(1.9 - .data$v))),
        pdoublesawed =  1 / (1 + exp(3.5*(1.6 - .data$v))),
        pdifficult = 0.7 / (1 + exp(4.4 - (2 * .data$v))),
        p = 0,
        cc_intc = dplyr::case_when( # Intercept for clearcutting model
          modelversion == "Talbot16" ~ 21.3,
          modelversion == "Brunberg07" ~ 24,# ref BRUNBERG 2007 is cc_intc = 24
          TRUE ~ 27.3),  # Brunberg 95

        cc_slp = dplyr::case_when( # Slope for clearcutting model. To be multiplied with mean tree size m3sub.
          modelversion == "Talbot16" ~ 157.92,
          modelversion == "Brunberg07" ~ 35, # ref Brunberg 2007 extra large harvesters: cc_slp = 35
          TRUE ~ 56  ) # Brunberg 95
        ) %>%
      dplyr::mutate(
        t2 = (.data$cc_intc + .data$cc_slp * .data$v + 28* .data$pdoublesawed + 15* .data$phindrance + 37* .data$pdifficult))

  # heureka t3 Additional harvesting time, cmin per tree ----
  stands_df <- stands_df %>%
    dplyr::mutate( t3 = 1.6)

  # wrap up ----
  stands_df <- stands_df %>%
    dplyr::mutate( ttot_cmin_tree = .data$t1 + .data$t2 + .data$t3) %>% # ttotal is centiminutes per tree, G0
    dplyr::mutate(
                  harv_G15min.tree = .data$ttot_cmin_tree * 1.3 / 100,
                  harv_G15min.ha = .data$harv_G15min.tree * .data$Nharv,
                  harv_G15h.ha = round(.data$harv_G15min.ha / 60, 3),
                  harv_G15min.m3 = round(.data$harv_G15min.ha / .data$Vharv, 3),
                  harv_m3.G15h = round(60 / .data$harv_G15min.m3, 3),
                  harv_stems.G15h = round(60 / .data$harv_G15min.tree, 1))  %>%
    dplyr::select( tidyselect::starts_with("harv_"), "t1", "t2", "t3", "ttot_cmin_tree", tidyselect::everything())


  if(verbose == FALSE){
    ttot <- stands_df %>% dplyr::select(tidyselect::starts_with("harv_G"), "harv_m3.G15h")
  } else {
    ttot <- stands_df
  }
  return(ttot)
}
