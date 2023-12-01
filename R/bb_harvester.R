


# Harvester time consumption model ----
#' t_harv_bb
#'
#' @description
#' `t_harv_bb()` estimate harvester time consumption in forest operations, according
#' to models developed by Torbjörn Brunberg (Skogforsk Sweden) and
#' later modified version by Bruce Talbod (NIBIO Norway)
#'
#' @details
#' variables to provide is listed under heading "3 Variables" in the Heureka
#' time consumption functions description
#' https://www.heurekaslu.se/w/images/8/8c/Time_consumption_harvester_and_forwarder.pdf
#'
#' @param stands_df A data frame having stand characteristics to be used in the
#' function. Can include all or any or none of the variables below. Variables in
#' stands_df will override variables in the function call.
#' @param Nharv Number of trees per hectare'
#' @param v mean volume m3fub of the harvested trees in the stand.
#' @param modelversion one of "Talbot16" or "Brunberg"
#' @param L slope class (1:5) where  1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\%
#' @param Y surface structure (1:5) where 5 is uneven
#' @param treatment one of : "clearcutting", "thinning"
#' @param Nres Number of trees left per hectare (used in thinning time calc)
#' @param thinningsystem one of: striproad, striproad_with_midfield_machine, striproad_with_midfield_chainsaw
#' @param thinningnumber one of: 1, 2,
#' @param rd relative diameter in thinning,
#' i.e. mean dia harvested / mean dia residual trees. BA weighted mean diameters.
#' @param p_init_spruce proportion of spruce before harvest
#' @param Pharv_broadLeaves proportion broadleaved trees of harvested trees
#' @param W width of strips between striproads (excluding striproad)
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
#' t_harv_bb(stands_df = testdata_heureka_clearcutting, modelversion = "Brunberg") %>% dplyr::glimpse()
#' t_harv_bb(stands_df = testdata_heureka_clearcutting, modelversion = "Talbot16") %>% dplyr::glimpse()
#' t_harv_bb(stands_df = testdata_heureka_thinning) %>% dplyr::glimpse()
#' t_harv_bb(stands_df = testdata_clearcut_Talbot16, verbose = TRUE) %>% dplyr::glimpse()

#'
t_harv_bb = function(stands_df, Nharv = 700, v = 0.4, modelversion = "Brunberg",
                     treatment = "clearcutting",
                     thinningsystem = NA_character_, thinningnumber = NA_real_,
                     rd = NA_real_, Nres = NA_real_, p_init_spruce = 1,
                     Pharv_broadLeaves = 0,
                     L = 2, Y = 2, W = 16, verbose = FALSE){

  # prepping data frame ----
  standvars <- colnames(stands_df)


  if(!('Nharv' %in% standvars)) { stands_df$Nharv <- Nharv}
  if(!('v' %in% standvars)) { stands_df$v <- v}
  if(!('L' %in% standvars)) { stands_df$L <- L}
  if(!('Y' %in% standvars)) { stands_df$Y <- Y}
  if(!('modelversion' %in% standvars)) { stands_df$modelversion <- modelversion}
  if(!('treatment' %in% standvars)) { stands_df$treatment <- treatment}

  if( "thinning" %in% unique(tolower( stands_df$treatment))) {
    if(!("thinningsystem" %in% standvars)){stands_df$thinningsystem <- thinningsystem}
    if(!("thinningnumber" %in% standvars)){stands_df$thinningnumber <- thinningnumber}
    if(!("rd" %in% standvars)){stands_df$rd <- rd}
    if(!('W' %in% standvars)) { stands_df$W <- W}
    if(!("p_init_spruce" %in% standvars)){stands_df$p_init_spruce <- p_init_spruce}
    if(!("Pharv_broadLeaves" %in% standvars)){stands_df$Pharv_broadLeaves <- Pharv_broadLeaves}
    if(!("Nres" %in% standvars)){stands_df$Nres <- Nres}
    stopifnot( all(unique(stands_df$thinningsystem) %in% c("striproad", "striproad_with_midfield_machine", "striproad_with_midfield_chainsaw", NA_character_)))
  }
  stands_df <- stands_df %>% dplyr::mutate(
    Vharv = v * Nharv
  )

  # heureka t1 Harvester driving time in clearcutting and thinning, cmin / tree ----
  if(!( c("thinning") %in% unique(tolower( stands_df$treatment)))) {
  ht1 <- stands_df %>%
    dplyr::select( "Nharv", "v",  "L", "Y") %>%
    dplyr::mutate(
                  Nharv =  trunk_rng(.data$Nharv, 200, 1500),
                  v = trunk_rng(.data$v, 0, 3),
                  L = trunk_rng(.data$L, 1, 4),
                  Y = trunk_rng(.data$Y, 1, 4),
                  K = 25.9, # Brunberg 2007, extra large harvester: K = 31
                  S = 13.3,
                  t1 = 10^6 / (.data$S * .data$Nharv * .data$K * (1 + (50/.data$Nharv) - 0.1*.data$Y - 0.1*.data$L)),
                  #cmin/trad Brunberg 2007 p5.
                  t1 = trunk_rng(.data$t1, 2, 20)
    ) %>%
    dplyr::select( "t1")
  } else {
      ht1 <- stands_df %>%
        dplyr::select( "Nharv", "v",  "L", "Y", "treatment", "thinningsystem", "W") %>%
        dplyr::mutate(
          Nharv = dplyr::case_when(
            .data$treatment == "clearcutting" ~ trunk_rng(.data$Nharv, 200, 1500),
            .data$treatment == "thinning" ~ trunk_rng(.data$Nharv, 400, 2000),
            TRUE ~ 0),
          v = trunk_rng(.data$v, 0, 3),
          L = trunk_rng(.data$L, 1, 4),
          Y = trunk_rng(.data$Y, 1, 4),
          K = dplyr::case_when( # "default speed factor" at even flat ground
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_machine" ~ 15.4,
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_chainsaw" ~ 20.2,
            .data$treatment == "thinning"  ~ 15.6,
            .data$treatment == "clearcutting" ~ 25.9,
            # Brunberg 2007, extra large harvester: K = 31
            TRUE ~ 25.9),
          S = dplyr::case_when( # work width ('Stråkbredd'), m
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_machine" ~ 2 * .data$W / 3,
            .data$treatment == "thinning" &
              .data$thinningsystem == "striproad_with_midfield_chainsaw" ~  2 * .data$W / 3,
            .data$treatment == "clearcutting" ~ 13.3,
            .data$treatment == "thinning"  ~ .data$W,
            TRUE ~ 13.3),
          t1 = 10^6 / (.data$S * .data$Nharv * .data$K * (1 + (50/.data$Nharv) - 0.1*.data$Y - 0.1*.data$L)),
          #cmin/trad Brunberg 2007 p5.
          t1 = trunk_rng(.data$t1, 2, 20)
        ) %>%
        dplyr::select( "t1")
    }


  # heureka t2 Felling and processing time, cmin per tree ----
  if(!( c("thinning") %in% unique(tolower( stands_df$treatment)))) {
    ht2 <-
      stands_df %>%
      dplyr::select(
        "treatment",
        "v",
        "modelversion") %>%
      dplyr::mutate(
        phindrance = 0.35/ (1 + exp(2.5*(1.9 - .data$v))),

        pdoublesawed =  1 / (1 + exp(3.5*(1.6 - .data$v))),
        pdifficult = 0.7 / (1 + exp(4.4 - (2 * .data$v))),

        p = 0,

        cc_intc = dplyr::case_when( # Intercept for clearcutting model
          modelversion == "Talbot16" ~ 21.3,
          TRUE ~ 27.3),  # Brunberg 95
        # BRUNBERG 2007 is 24

        cc_slp = dplyr::case_when( # Slope for clearcutting model. To be multiplied with mean tree size m3sub.
          modelversion == "Talbot16" ~ 157.92,
          TRUE ~ 56  ) # Brunberg 95
        # Brunberg 2007 extra large harvesters: cc_slp = 35
        ) %>%
      dplyr::mutate(
        t21 = (.data$cc_intc + .data$cc_slp * .data$v + 28* .data$pdoublesawed + 15* .data$phindrance + 37* .data$pdifficult)) %>% # Clearcutting model
      dplyr::mutate( t2 = .data$t21) %>%
      dplyr::select( "t2")

  } else {
   ht2 <-
    stands_df %>%
    dplyr::select(
      "treatment",
      "v",
      "thinningnumber",
      "thinningsystem",
      "p_init_spruce",
      "Pharv_broadLeaves",
      "modelversion",
      "rd",
      "Nres") %>%
    dplyr::mutate(
                  phindrance = dplyr::case_when(
                    .data$treatment == "thinning" & .data$v <= 0.2 ~ 0,
                    TRUE ~ 0.35/ (1 + exp(2.5*(1.9-v)))),

                  pdoublesawed = dplyr::case_when(
                    .data$treatment == "thinning" & v <= 0.2 ~ 0,
                    TRUE  ~ 1 / (1 + exp(3.5*(1.6-v)))),
                  pdifficult = dplyr::case_when(
                    .data$treatment == "thinning" & .data$v <= 0.2 ~ 0,
                    TRUE ~ 0.7 / (1 + exp(4.4 - (2*v)))),

                  p = dplyr::case_when(
                    .data$treatment == "thinning" &  .data$thinningnumber == 1 & .data$v <= 0.2 ~ .data$p_init_spruce,
                    .data$treatment == "thinning" &  .data$thinningnumber == 2 & .data$v <= 0.2 ~ 0.5 * .data$p_init_spruce,
                    TRUE ~ 0),

                  cc_intc = dplyr::case_when( # Intercept for clearcutting model
                    modelversion == "Talbot16" ~ 21.3,
                    TRUE ~ 27.3),  # Brunberg 95
                  # BRUNBERG 2007 is 24

                  cc_slp = dplyr::case_when( # Slope for clearcutting model. To be multiplied with mean tree size m3sub.
                    modelversion == "Talbot16" ~ 157.92,
                    TRUE ~ 56  ), # Brunberg 95
                  # Brunberg 2007 extra large harvesters: cc_slp = 35

                  t21 = dplyr::case_when( #tmp time consumption estimate
                    .data$treatment == "thinning" & .data$v <= 0.2 ~
                      (v * (78 * .data$p + 89) + .data$Nres *
                         (0.0025 * .data$p + 0.0019) + 20.3) + 2.3 * .data$Pharv_broadLeaves, # Thinning model
                    TRUE  ~
                      (cc_intc + cc_slp*v + 28*pdoublesawed +
                         15*phindrance + 37*pdifficult)), # Clearcutting model

                  thinningtypecorr = dplyr::case_when(
                    ((.data$treatment == "thinning") & (v <= 0.2)  & (rd > 0.95) & (rd <= 1)) ~
                      -1.3, # Uniform thinning
                    ((treatment == "thinning") & (v <= 0.2) & (rd > 1)) ~
                      -16*(pmin(rd, 1.1) - 1),  # Thinning from above
                    TRUE ~ 0),

                  thinningsyscorr = dplyr::case_when(
                    .data$treatment == "thinning" &
                      (v <= 0.2) &
                      thinningsystem == "striproad_with_midfield_machine" ~
                      3.4 * 0.3,
                    .data$treatment == "thinning" &
                      (v <= 0.2) &
                      thinningsystem == "striproad_with_midfield_chainsaw" ~
                      8.3 * 0.17,
                    TRUE ~ 0),
                  t2 = .data$t21 + .data$thinningtypecorr + .data$thinningsyscorr
    ) %>%
    dplyr::select( "t2")
}
  # heureka t3 Additional harvesting time, cmin per tree ----
  ht3 <- stands_df %>%
    dplyr::select( "treatment") %>%
    dplyr::mutate(t3 = dplyr::if_else(.data$treatment == "clearcutting", 1.6, 4.3)) %>%
    dplyr::select( "t3")

  # wrap up ----
  ttot <- dplyr::bind_cols(
    ht1, ht2, ht3) %>%
    dplyr::rowwise() %>%
    dplyr::mutate( ttotal = sum(dplyr::c_across(tidyselect::everything()))) %>% # ttotal is centiminutes per tree
        dplyr::bind_cols(stands_df) %>%
    dplyr::mutate(
                  harv_G15min.tree = .data$ttotal * 1.3 / 100,
                  harv_G15min.ha = .data$harv_G15min.tree * .data$Nharv,
                  harv_G15h.ha = round(.data$harv_G15min.ha / 60, 3),
                  harv_G15min.m3 = round(.data$harv_G15min.ha / .data$Vharv, 3),
                  harv_m3.G15h = round(60 / .data$harv_G15min.m3, 3)) %>%
    dplyr::select( -tidyselect::starts_with("tt") #,
                  #-tidyselect::num_range("t", 1:3)
    ) %>%
    dplyr::select( tidyselect::starts_with("harv_"), tidyselect::everything())


  if(verbose == FALSE){
    ttot <- ttot %>% dplyr::select(tidyselect::starts_with("harv_G"), "harv_m3.G15h")
  }
  return(ttot)
}
