


# Harvester time consumption model ----
#' t_harv_thinning_bb
#'
#' @description
#' `t_harv_thinning_bb()` estimates harvester time consumption in forest operations, according
#' to models presented in the swedish Heureka description
#'
#' @details
#' Variables to provide is listed under heading "3 Variables" in the Heureka
#' time consumption functions description
#' https://www.heurekaslu.se/w/images/8/8c/Time_consumption_harvester_and_forwarder.pdf
#' The thinning harvester model is the Heureka clearcut model, being a
#' slightly modified version of Brunberg1997
#' NB: variables listed as arguments may also be provided in the "stands_df" dataset.
#'
#'
#' @param stands_df A data frame having stand and operational characteristics to be used in the
#' function. May include all the parameters set in the function call except "verbose"
#' @param Stems_ha Number of stems per hectare,
#' @param v mean volume m3fub of the trees in the stand.
#' @param p_init_spruce proportion of spruce before harvest
#' @param L slope class (1:5) where  1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\%
#' @param Y surface structure (1:5) where 5 is uneven
#' @param harvest_strength_pct  share of basal area to be cut. Range 0-100
#' @param p_init_spruce proportion of spruce before harvest
#' @param modelversion one of "Brunberg95" or"Brunberg07", "Talbot16"
#' @param thinningsystem one of: striproad, striproad_with_midfield_machine, striproad_with_midfield_chainsaw
#' @param thinningnumber one of: 1, 2, (Applies also to bledningshogst, 1 is first, 2 is later bledning where striproads exist)
#' @param rd relative diameter in thinning,
#' i.e. mean dia harvested / mean dia residual trees. BA weighted mean diameters.
#' @param Pharv_broadLeaves proportion broadleaved trees of harvested trees
#' @param sr_sp striproad spacing centre to centre
#' @param sr_w striproad widt: 2 * distance striporoad centreline to nearest tree
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
#' t_harv_thinning_bb(stands_df = testdata_heureka_thinning, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_thinning_bb(stands_df = testdata_clearcut_forw, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_thinning_bb(stands_df = testdata_heureka_clearcutting,  verbose = TRUE) %>% dplyr::glimpse()
t_harv_thinning_bb = function(stands_df = NULL, Stems_ha = 1200, v = 0.4,
                              harvest_strength_pct = 30,  p_init_spruce = 0.95, L = 2, Y = 2,
                     modelversion = "Brunberg97",
                     Pharv_broadLeaves = 0,
                     thinningsystem = c("striproad_with_midfield_machine", "striproad_with_midfield_chainsaw", "striproad")[3], thinningnumber = 1,
                     rd = 0.9, sr_sp = 20, sr_w = 4, verbose = FALSE){
  if(is.null(stands_df)){
    stands_df <- data.frame(Stems_ha = Stems_ha, v = v)
  }
  stopifnot(is.data.frame(stands_df))
  # prepping data frame ----
  # If variables are specified in df; keep them, if not; use as defined in function
  standvars <- colnames(stands_df)
  stopifnot(c("v", "Stems_ha") %in% standvars)


  if(!('Stems_ha' %in% standvars)) { stands_df$Stems_ha <- Stems_ha}
  if(!('v' %in% standvars)) { stands_df$v <- v}
  if(!('L' %in% standvars)) { stands_df$L <- L}
  if(!('Y' %in% standvars)) { stands_df$Y <- Y}
  if(!('modelversion' %in% standvars)) { stands_df$modelversion = modelversion}
  if(!('harvest_strength_pct' %in% standvars)) { stands_df$harvest_strength_pct = harvest_strength_pct}
  if(!("rd" %in% standvars)){  stands_df$rd <- rd  }
  if(!("sr_sp" %in% standvars)){  stands_df$sr_sp <- sr_sp  }
  if(!("sr_w" %in% standvars)){  stands_df$sr_w <- sr_w  }
  if(!("thinningnumber" %in% standvars)){  stands_df$thinningnumber <- thinningnumber  }
  if(!('Nharv' %in% standvars)) { stands_df$Nharv = stands_df$Stems_ha * stands_df$harvest_strength_pct/100 / stands_df$rd}
  if(!("thinningsystem" %in% standvars)){stands_df$thinningsystem <- thinningsystem}
  if(!("thinningnumber" %in% standvars)){stands_df$thinningnumber <- thinningnumber}
  if(!('W' %in% standvars)) { stands_df$W <- stands_df$sr_sp  - stands_df$sr_w}
  if(!("p_init_spruce" %in% standvars)){stands_df$p_init_spruce <- p_init_spruce}
  if(!("Pharv_broadLeaves" %in% standvars)){stands_df$Pharv_broadLeaves <- Pharv_broadLeaves}

    stands_df$treatment = "thinning"

    stands_df <- stands_df %>% dplyr::mutate(
      Nres = .data$Stems_ha - .data$Nharv
      , V.init = .data$Stems_ha * .data$v
      , Vharv = .data$V.init  * .data$harvest_strength_pct/100
      , v_hrv = .data$v * stands_df$rd * sqrt(stands_df$rd) # Check how the diff in relative diameter affects the diff in relative volume
      , v_tr = trunk_rng(.data$v_hrv, 0, 3)
      , L = trunk_rng(.data$L, 1, 4)
      , Y = trunk_rng(.data$Y, 1, 4)
      , p_init_spruce = trunk_rng(.data$p_init_spruce, 0, 1)
      , modelversion = modelversion
      , striproad_exist = dplyr::if_else(.data$thinningnumber == 1,  FALSE, TRUE)
      )

stopifnot( all(unique(stands_df$thinningsystem) %in% c("striproad", "striproad_with_midfield_machine", "striproad_with_midfield_chainsaw")))


  # heureka t1 Harvester driving time in clearcutting and thinning, cmin / tree ----
 stands_df <- stands_df %>%
        dplyr::mutate(
          Nharv_tr = trunk_rng(.data$Nharv, 400, 2000),
          K = dplyr::case_when( # "default speed factor" at even flat ground
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_machine" ~ 15.4,
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_chainsaw" ~ 20.2,
            .data$treatment == "thinning"  ~ 15.6,
            .data$treatment == "clearcutting" ~ 25.9,
            TRUE ~ 25.9),
          S = dplyr::case_when( # work width (= 'Stråkbredd'), m
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_machine" ~ 2 * .data$W / 3,
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_chainsaw" ~  2 * .data$W / 3,
            .data$treatment == "clearcutting" ~ 13.3,
            .data$treatment == "thinning"  ~ .data$W,
            TRUE ~ 13.3),
          t1 = 10^6 / (.data$S * .data$Nharv_tr * .data$K * (1 + (50/.data$Nharv_tr) - 0.1*.data$Y - 0.1*.data$L)),
          #cmin/trad Brunberg 2007 p5.
          t1 = trunk_rng(.data$t1, 2, 20)
        )


  # heureka t2 Felling and processing time, cmin per tree ----
stands_df <- stands_df %>%
    dplyr::mutate(
                  phindrance = dplyr::case_when(
                    .data$treatment == "thinning" & .data$v_tr <= 0.2 ~ 0,
                    TRUE ~ 0.35/ (1 + exp(2.5*(1.9 - v_tr)))),

                  pdoublesawed = dplyr::case_when(
                    .data$treatment == "thinning" & v_tr <= 0.2 ~ 0,
                    TRUE  ~ 1 / (1 + exp(3.5*(1.6 - v_tr)))),
                  pdifficult = dplyr::case_when(
                    .data$treatment == "thinning" & .data$v_tr <= 0.2 ~ 0,
                    TRUE ~ 0.7 / (1 + exp(4.4 - (2 * v_tr)))),

                  p = dplyr::case_when(
                    .data$treatment == "thinning" &  .data$thinningnumber == 1 & .data$v_tr <= 0.2 ~ .data$p_init_spruce,
                    .data$treatment == "thinning" &  .data$thinningnumber == 2 & .data$v_tr <= 0.2 ~ 0.5 * .data$p_init_spruce,
                    TRUE ~ 0),

                  cc_intc = dplyr::case_when( # Intercept for clearcutting model
                    modelversion == "Talbot16" ~ 21.3,
                    modelversion == "Brunberg07" ~ 24,  # ref: BRUNBERG 2007 p6
                    TRUE ~ 27.3),  # Brunberg 95
                  # BRUNBERG 2007 is 24

                  cc_slp = dplyr::case_when( # Slope for clearcutting model. To be multiplied with mean tree size m3sub.
                    modelversion == "Talbot16" ~ 157.92,
                    modelversion == "Brunberg07" ~ 35,# ref Brunberg 2007 extra large harvesters: cc_slp = 35
                    TRUE ~ 56  ), # Brunberg 95


                  t21 = dplyr::case_when( #tmp time consumption estimate
                    .data$treatment == "thinning" & .data$v_tr <= 0.2 ~ # should be 0.2, no 0.35
                      (v_tr * (78 * .data$p + 89) + .data$Nres *
                         (0.0025 * .data$p + 0.0019) + 20.3) + 2.3 * .data$Pharv_broadLeaves, # Thinning model
                    TRUE  ~
                      (cc_intc + cc_slp*v_tr + 28*pdoublesawed +
                         15*phindrance + 37*pdifficult)), # Clearcutting model

                  thinningtypecorr = dplyr::case_when(
                    ((.data$treatment == "thinning") & (v_tr <= 0.2)  & (rd > 0.95) & (rd <= 1)) ~
                      -1.3, # Uniform thinning
                    ((treatment == "thinning") & (v_tr <= 0.2) & (rd > 1)) ~
                      -16*(pmin(rd, 1.1) - 1),  # Thinning from above
                    TRUE ~ 0),

                  thinningsyscorr = dplyr::case_when(
                    .data$treatment == "thinning" &
                      (v_tr <= 0.2) &
                      thinningsystem == "striproad_with_midfield_machine" ~
                      3.4 * 0.3,
                    .data$treatment == "thinning" &
                      (v_tr <= 0.2) &
                      thinningsystem == "striproad_with_midfield_chainsaw" ~
                      8.3 * 0.17,
                    TRUE ~ 0),
                  t2 = .data$t21 + .data$thinningtypecorr + .data$thinningsyscorr
                  )




  # heureka t3 Additional harvesting time, cmin per tree ----
  stands_df <- stands_df %>%
    dplyr::mutate(t3 =  4.3) # Heureka chpt 4.5 Additional time in harvesting

  # wrap up ----
    stands_df <- stands_df %>%
    dplyr::mutate(ttot_cmin_tree = .data$t1 + .data$t2 + .data$t3,
                  harv_G15min.tree = .data$ttot_cmin_tree * 1.3 / 100,
                  harv_G15min.ha = .data$harv_G15min.tree * .data$Nharv,
                  harv_G15h.ha = round(.data$harv_G15min.ha / 60, 3),
                  harv_G15min.m3 = round(.data$harv_G15min.ha / .data$Vharv, 3),
                  harv_m3.G15h = round(60 / .data$harv_G15min.m3, 3),
                  harv_stems.G15h = round(60 / .data$harv_G15min.tree, 1)) %>%
    dplyr::select( tidyselect::starts_with("harv_"), tidyselect::everything())


  if(verbose == FALSE){
    stands_df <- stands_df %>% dplyr::select(tidyselect::starts_with("harv_G"), "harv_m3.G15h")
  }
  return(stands_df)
}
