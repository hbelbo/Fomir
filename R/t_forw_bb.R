# Forwarder time consumption model -----
#' t_forw_bb
#'
#' #' @description
#' `t_forw_bb()` estimate forwarder time consumption in forest operations, according
#' to models presented in the swedish Heureka description,
#' and a later modified version by Bruce Talbod (NIBIO Norway)
#'
#' @param stands_df
#' A data frame having stand characteristics according to the
#' variables listed under heading "3 Variables" in the Heureka time consumption functions description
#' https://www.heurekaslu.se/w/images/8/8c/Time_consumption_harvester_and_forwarder.pdf
#'
#' Mandatory variables in stands_df, with default values:
#'   v = 0.3; mean volume m3fub of the harvested trees in the stand
#'   Nharv = 1000; Number of trees per hectare
#'  Optional variables, of which default values is set as first value:
#'   nbAssortments = 4; Number of qualities and species. Afect t7 ForwarderSortingTime
#'   treatment = one of : "clearcutting", "thinning"
#'   L = 2, slope class (1:5) where  1 is flat (0-10%), 2 is 10-20%, 3 is 20-33%, 4 is 33-50%, 5 is >50%
#'   Y = 2, surface structure (1:5) where 5 is uneven
#'   D = 300, Terrain transport distance (one way average)
#' @param forw_size can be "small", "medium", "large"
#' Should also take "xl" and "xxl" but not implemented
#'
#' @param modelversion one of "Brunberg" | "Talbot16"
#' @param treatment one of "thinning" | "clearcutting"
#' @param harvest_strength_pct  share of basal area to be cut. Range 0-100
#' @param verbose if TRUE: more intermediate time consumption sstimates will be returned
#'
#' @return a data frame with forwarder time consumption (minutes per hectare)
#' \strong{variables in output df:}
#' \describe{
#' \item{forw_G15h.ha}{Forwarder G15 hours per hectare}}
#' @export
#'
#' @examples
#' t_forw_bb(stands_df = testdata_heureka_forwarding, verbose = TRUE) %>% dplyr::glimpse()
#' t_forw_bb(stands_df = testdata_heureka_thinning, verbose = TRUE) %>% dplyr::glimpse()
#' t_forw_bb(stands_df = testdata_heureka_clearcutting, verbose = TRUE) %>% dplyr::glimpse()
t_forw_bb <- function(stands_df, forw_size = "large", modelversion = c("Brunberg04", "Talbot16")[2],
                      treatment = c("thinning", "clearcutting")[2],
                      harvest_strength_pct = 100, #pct basal area
                      verbose = FALSE) {
  # stands_df = heureka_testdata_forwarding
  # forw_size = "large"; modelversion = "Talbot16"
  # prepping data frame ----

  stopifnot(is.data.frame(stands_df))
  stopifnot(c("v", "Stems_ha") %in% names(stands_df))

  standvars <- colnames(stands_df)

  # preparig the dataset ----
  if (!("forw_size" %in% standvars)){stands_df$forw_size = forw_size }
  if (!("nbAssortments" %in% standvars)){stands_df$nbAssortments = 4 }
  if (!("L" %in% standvars)){stands_df$L = 2 }
  if (!("Y" %in% standvars)){stands_df$Y = 2 }
  if (!("D" %in% standvars)){stands_df$D = 200 }
  if (!("D_basveg" %in% standvars)){stands_df$D_basveg = 200 }
  if (!("treatment" %in% standvars)){stands_df$treatment = treatment }
  if (!("harvest_strength_pct" %in% standvars)){stands_df$harvest_strength_pct = harvest_strength_pct }
  if (!("modelversion" %in% standvars)){stands_df$modelversion = modelversion }




  # Removal per hectare
  stands_df$Vharv = stands_df$Stems_ha * stands_df$v * stands_df$harvest_strength_pct/100
  stands_df$Nharv = stands_df$Vharv / stands_df$v



  # heureka t4 terminal time forwarder (G15min / m3fub) -----
  #    for loading, driving during loading and unloading forwarder

  stands_df <- stands_df %>%
    dplyr::mutate(
      a = dplyr::case_when(
        .data$treatment == "thinning"  ~ -43,
        TRUE ~ 5.7),
      b = dplyr::case_when(
        .data$treatment == "thinning"  ~ 25.9,
        TRUE ~ 11.45),
      K1 = dplyr::case_when(
        .data$modelversion == "Talbot16" ~ 1.4,
        TRUE ~ 1), # Brunberg 04
      K2 = dplyr::case_when(
        .data$treatment == "clearcutting" & .data$forw_size == "small"  ~ 1.04,
        .data$treatment == "clearcutting" & .data$forw_size == "medium" ~ 0.86,
        .data$treatment == "clearcutting" & .data$forw_size == "big"  ~ 0.73,
        .data$treatment == "thinning" & .data$forw_size == "small"  ~ 1.18,
        .data$treatment == "thinning" & .data$forw_size == "medium" ~ 0.67,
        .data$treatment == "thinning" & .data$forw_size == "big"  ~ 0.67,
        TRUE ~ 1.04),
      # Vharv2 =  Vharv, HeurekaSLU used un-trunkated stand volume, probably a mistake?
      Vharv2 =   dplyr::case_when( # Trunkating to model range
        .data$treatment == "thinning" ~ trunk_rng(.data$Vharv, 25, 125),
        TRUE ~  trunk_rng(.data$Vharv, 50, 350) ), # clearcutting
      t4 = .data$K1 * ((.data$a + .data$K2*.data$Vharv2 + .data$b*sqrt(.data$Vharv2))/.data$Vharv2 )
    )



  # heureka t5 driving time forwarder (G15-min / m3fub) -----
  stands_df <- stands_df  %>%
    dplyr::mutate(
      speed = (75 - (8.2 * .data$Y) - (1.4 * .data$L^2)), # Brunberg 04; meter / G15-min
      speed = dplyr::if_else(.data$treatment == "clearcutting", .data$speed , (0.85 * .data$speed)),
      c_cap = dplyr::case_when( # Volume capacity, m3fub per load
        .data$forw_size == "small" ~ 9.5,
        .data$forw_size == "medium" ~ 13.6,
        .data$forw_size == "large" ~ 17.9,
        TRUE ~ 17.9),
      t5 = (2*.data$D / (.data$speed * .data$c_cap)))

  # heureka_t6 Assortment dependent time, G15min / m3fub -----
  stands_df <- stands_df %>%
    dplyr::mutate(
      vtr = trunk_rng(.data$v, 0, 0.5),
      t6 = 0.05 - .data$vtr    # Brunberg 2004
    )

  # heureka t7 forwarder sorting time, G15min / m3fub  ----
  stands_df <- stands_df  %>%
    dplyr::mutate( t7 = -0.1 + 0.1 * .data$nbAssortments )



  # heureka t8 forwarder additional time, G15 minutes per hectare ----

  # return forwarder additional time, G15 minutes per hectare
  #  assuming 1.5 minute per load, and calculating loads per hectare
  stands_df <-  stands_df  %>%
    dplyr::mutate(
      c_cap = dplyr::case_when(
        forw_size == "small" ~ 9.5,
        forw_size == "medium" ~ 13.6,
        forw_size == "large" ~ 17.9,
        TRUE ~ 17.9),
      nbLoads = floor((.data$Vharv / .data$c_cap) + .99),
      t8 = 1.5 * .data$nbLoads)

  # merging forwarder times tt4 ... tt7 ------

  stands_df <-  stands_df  %>%
    dplyr::rowwise() %>%
    dplyr::mutate( t4t7 = .data$t4 + .data$t5 + .data$t6 + .data$t7 ) %>%
    dplyr::mutate( forw_G15min.ha = .data$t4t7 * .data$Vharv + .data$t8) %>%
    dplyr::mutate( forw_G15min.m3 = round(.data$forw_G15min.ha / .data$Vharv, 1),
                   forw_G15h.ha = round((.data$forw_G15min.ha / 60), 2),
                   forw_m3.G15h = round( 60 / (.data$forw_G15min.ha / .data$Vharv), 2)) %>%
    #dplyr::select( #-tidyselect::starts_with("tt"),
                   #-.data$ttt,
                   #-tidyselect::num_range("t", 4:8)) %>%
    dplyr::select( tidyselect::starts_with("forw_"), tidyselect::everything())
if(verbose == FALSE){
  stands_df <- stands_df %>% dplyr::select(tidyselect::starts_with("forw_G"), "forw_m3.G15h")
}
  return(stands_df)
}

