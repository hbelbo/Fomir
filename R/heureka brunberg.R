# trunc_vls ---------
#' trunc_vls
#'
#' @param x
#' @param min_val
#' @param max_val
#'
#' @return x truncated to within min_val and max_val
#' @export
#'
#' @examples trunc_vls(1:10, 3, 6)
trunc_vls <- function(x, min_val, max_val){
  x[x > max_val] <- max_val
  x[x < min_val] <- min_val
  return(x)
}


# Harvester time consumption model ----
#' t_harv
#'
#' @param stands_df
#' A data frame having stand characteristics according to the
#' variables listed under heading "3 Variables" in the Heureka time consumption functions description
#' https://www.heurekaslu.se/w/images/8/8c/Time_consumption_harvester_and_forwarder.pdf
#'
#' Mandatory variables in stands_df:
#'   v = mean volume m3fub of the harvested trees in the stand.
#'   Nharv = Number of trees per hectare
#' Optional variables, of which default values is set as first value here in the description
#'  for clearcutting & thinning:
#'  L = 2, slope class (1:5) where 5 is steep
#'  Y = 2, surface structure (1:5) where 5 is uneven
#'  treatment = one of : "clearcutting", "thinning"
#'  Nres = 0; Number of trees left per hectare (used in thinning time calc)
#'  thinningsystem = one of: striproad, striproad_with_midfield_machine, striproad_with_midfield_chainsaw
#'  thinningnumber = one of: 1, 2,
#'  rd = relative diameter in thinning = 0.9, i.e. mean dia harvested / mean dia residual trees. BA weighted mean diameters.
#'  p_init_spruce = 0.9, proportion of spruce before harvest
#'  Pharv_broadLeaves = .9 proportion broadleaved trees of harvested trees
#'  W = 16, width of strips between striproads (excluding striproad)

#' @param modelversion one of "Talbot16"|"Brunberg"
#'
#' @return a data frame having the estimated time consumption cmin per tre (ttot), and also:
#' harv_G15min.tree (harvest time, G15 minutes per tree)
#' harv_G15h.ha (harvest time, G15 hours per hectare)
#' harv_G15min.m3 (harvest time, G15 minutes per m3)
#' harv_m3.G15h (productivity, m3 per G15-hour)
#'
#' @export
#'
#' @examples
#' t_harv(stands_df = heureka_testdata_clearcutting, modelversion = "Brunberg") %>% dplyr::glimpse()
#' t_harv(stands_df = heureka_testdata_clearcutting, modelversion = "Talbot16") %>% dplyr::glimpse()
#' t_harv(stands_df = heureka_testdata_thinning, modelversion = "Brunberg") %>% dplyr::glimpse()
#' t_harv(stands_df = heureka_testdata_thinning, modelversion = "Talbot16") %>% dplyr::glimpse()
#' t_harv(stands_df = stand_data_norsk_prodnorm) %>% dplyr::glimpse()
#' t_harv(stands_df = heureka_testdata_clearcutting) %>% dplyr::glimpse()
#'
t_harv = function(stands_df, modelversion = "Talbot16"){

  # prepping data frame ----
  stands_df = stands_df %>%
    dplyr::mutate(.,
      treatment      = if ( exists('treatment', where = .)) treatment else "clearcutting",
      thinningsystem = if ( exists('thinningsystem', where = .)) thinningsystem else "striproad",
      thinningnumber = if ( exists('thinningnumber', where = .)) thinningnumber else 1,
      rd             = if ( exists('rd', where = .)) rd else 0.9,
      p_init_spruce  = if ( exists('p_init_spruce', where = .)) p_init_spruce else 1,
      Pharv_broadLeaves = if ( exists('Pharv_broadLeaves', where = .)) Pharv_broadLeaves else  0.0,
      L = if ( exists('L', where = .)) L else 2,
      Y = if ( exists('Y', where = .)) Y else 2,
      W = if ( exists("W", where = .)) W else 16,
      Nres = if ( exists('Nres', where = .)) Nres else dplyr::if_else((treatment == "thinning"), Nharv/3, 0),
      Vharv = Nharv * v
    ) %>%
    dplyr::mutate(.,
      modelversion = modelversion )

  # heureka t1 Harvester driving time in clearcutting and thinning, cmin / tree ----
  ht1 <- stands_df %>%
    dplyr::select(., Nharv, v,  L, Y, treatment, thinningsystem, W) %>%
    dplyr::mutate(.,
      Nharv = dplyr::case_when(
        treatment == "clearcutting" ~ trunc_vls(Nharv, 200, 1500),
        treatment == "thinning" ~ trunc_vls(Nharv, 400, 2000),
        TRUE ~ 0),
                      v = trunc_vls(v, 0, 3),
      L = trunc_vls(L, 1, 4),
      Y = trunc_vls(Y, 1, 4),
      K = dplyr::case_when( # "default speed factor" at even flat ground
        treatment == "thinning" &
          thinningsystem == "striproad_with_midfield_machine" ~ 15.4,
        treatment == "thinning" &
          thinningsystem == "striproad_with_midfield_chainsaw" ~ 20.2,
        treatment == "thinning"  ~ 15.6,
        treatment == "clearcutting" ~ 25.9,
        # Brunberg 2007, extra large harvester: K = 31
        TRUE ~ 25.9),
      S = dplyr::case_when( # work width ('Stråkbredd'), m
        treatment == "thinning" &
          thinningsystem == "striproad_with_midfield_machine" ~ 2 * W / 3,
        treatment == "thinning" &
          thinningsystem == "striproad_with_midfield_chainsaw" ~  2 * W / 3,
        treatment == "clearcutting" ~ 13.3,
        treatment == "thinning"  ~ W,
        TRUE ~ 13.3),
      t1 = 10^6 / (S * Nharv * K * (1 + (50/Nharv) - 0.1*Y - 0.1*L)),
      #cmin/trad Brunberg 2007 p5.
      t1 = trunc_vls(t1, 2, 20)
    ) %>%
    dplyr::select(., t1)


  # heureka t2 Felling and processing time, cmin per tree ----
  ht2 <-
    stands_df %>%
      dplyr::select(.,
        treatment,
        v,
        thinningnumber,
        thinningsystem,
        p_init_spruce,
        Pharv_broadLeaves,
        modelversion,
        rd,
        Nres) %>%
    dplyr::mutate(.,
      phindrance = dplyr::case_when(
        treatment == "thinning" & v <= 0.2 ~ 0,
        TRUE ~ 0.35/ (1 + exp(2.5*(1.9-v)))),

      pdoublesawed = dplyr::case_when(
        treatment == "thinning" & v <= 0.2 ~ 0,
        TRUE  ~ 1 / (1 + exp(3.5*(1.6-v)))),
      pdifficult = dplyr::case_when(
        treatment == "thinning" & v <= 0.2 ~ 0,
        TRUE ~ 0.7 / (1 + exp(4.4 - (2*v)))),

      p = dplyr::case_when(
        treatment == "thinning" &  thinningnumber == 1 & v <= 0.2 ~ p_init_spruce,
        treatment == "thinning" &  thinningnumber == 2 & v <= 0.2 ~ 0.5 * p_init_spruce,
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
        treatment == "thinning" & v <= 0.2 ~
          (v * (78 * p + 89) + Nres *
             (0.0025 * p + 0.0019) + 20.3) + 2.3 * Pharv_broadLeaves, # Thinning model
        TRUE  ~
          (cc_intc + cc_slp*v + 28*pdoublesawed +
             15*phindrance + 37*pdifficult)), # Clearcutting model

      thinningtypecorr = dplyr::case_when(
        ((treatment == "thinning") & (v <= 0.2)  & (rd > 0.95) & (rd <= 1)) ~
          -1.3, # Uniform thinning
        ((treatment == "thinning") & (v <= 0.2) & (rd > 1)) ~
          -16*(pmin(rd, 1.1) - 1),  # Thinning from above
        TRUE ~ 0),

      thinningsyscorr = dplyr::case_when(
        treatment == "thinning" &
          (v <= 0.2) &
          thinningsystem == "striproad_with_midfield_machine" ~
            3.4 * 0.3,
        treatment == "thinning" &
          (v <= 0.2) &
          thinningsystem == "striproad_with_midfield_chainsaw" ~
            8.3 * 0.17,
        TRUE ~ 0),
      t2 = t21 + thinningtypecorr + thinningsyscorr
    ) %>%
    dplyr::select(., t2)

  # heureka t3 Additional harvesting time, cmin per tree ----
  ht3 <- stands_df %>%
    dplyr::select(., treatment) %>%
    dplyr::mutate(., t3 = dplyr::if_else(treatment == "clearcutting", 1.6, 4.3)) %>%
    dplyr::select(., t3)

  # wrap up ----
  ttot <- dplyr::bind_cols(
    ht1, ht2, ht3) %>%
    dplyr::mutate(., ttotal = rowSums(.)) %>% # ttotal is centiminutes per tree
    dplyr::bind_cols(., stands_df) %>%
    dplyr::mutate(.,
      harv_G15min.tree = ttotal * 1.3 / 100,
      harv_G15min.ha = harv_G15min.tree * Nharv,
      harv_G15h.ha = round(harv_G15min.ha / 60, 3),
      harv_G15min.m3 = round(harv_G15min.ha / Vharv, 3),
      harv_m3.G15h = round(60 / harv_G15min.m3, 3)) %>%
    dplyr::select(., -tidyselect::starts_with("tt") #,
                  #-tidyselect::num_range("t", 1:3)
                  ) %>%
    dplyr::select(., tidyselect::starts_with("harv_"), tidyselect::everything())
    return(ttot)
}


# Forwarder time consumption model -----
#' t_forw
#'
#' @param stands_df A data frame of stands for which forw time consumption is to
#' be estimated.
#' Mandatory variables in stands_df, with default values:
#'   v = 0.3; mean volume m3fub of the harvested trees in the stand
#'   Nharv = 1000; Number of trees per hectare
#'  Optional variables, of which default values is set as first value:
#'   nbAssortments = 4; Number of qualities and species. Afect t7 ForwarderSortingTime
#'   treatment = one of : "clearcutting", "thinning"
#'   L = 2, slope class (1:5) where 5 is steep
#'   Y = 2, surface structure (1:5) where 5 is uneven
#'   D = 300, Terrain transport distance (one way average)
#' @param forw_size can be "small", "medium", "large"
#' Should also take "xl" and "xxl" but not implemented
#'
#' @param modelversion one of "Brunberg" | "Talbot16"
#'
#'
#' @return a data frame with forwarder time consumption (minutes per hectare)
#' @export
#'
#' @examples
#' t_forw(stands_df = heureka_testdat_forwarding, forw_size = "medium", modelversion = "Brunberg") %>% glimpse()
#' t_forw(stands_df = heureka_testdat_forwarding, forw_size = "medium", modelversion = "Talbot16") %>% glimpse()
#' t_forw(stands_df = heureka_testdat_thinning) %>% glimpse()
#' t_forw(stands_df = heureka_testdata_clearcutting) %>% glimpse()
#' t_forw(stands_df = testdf_volume) %>% glimpse()
#'
t_forw <- function(stands_df, forw_size = "large", modelversion = "Talbot16") {

  # preparig the dataset ----
  stands_df = stands_df %>%
    dplyr::mutate(.,
      Nharv  = if (exists('Nharv', where = .)) Nharv else 1000,
      v = if (exists('v', where = .)) v else 0.3,
      nbAssortments = if (exists('nbAssortments', where = .)) nbAssortments else 4,
      treatment = if (exists('treatment', where = .)) treatment else "clearcutting",
      L = if (exists('L', where = .)) L else 2,
      Y = if (exists('Y', where = .)) Y else 2,
      D = if (exists('D', where = .)) D else 500,
      Vharv = Nharv * v  ) %>%
    dplyr::mutate(.,
      forw_size = forw_size,
      modelversion = modelversion)


  # heureka t4 terminal time forwarder (G15min / m3fub) -----
  #    for loading, driving during loading and unloading forwarder

  tt4 <- stands_df %>%
    dplyr::select(., Vharv, treatment, forw_size, modelversion) %>%
    dplyr::mutate(.,
      a = dplyr::case_when(
        treatment == "thinning"  ~ -43,
        TRUE ~ 5.7),
      b = dplyr::case_when(
        treatment == "thinning"  ~ 25.9,
        TRUE ~ 11.45),
      K1 = dplyr::case_when(
        modelversion == "Talbot16" ~ 1.4,
        TRUE ~ 1), # Brunberg 04
      K2 = dplyr::case_when(
        treatment == "clearcutting" & forw_size == "small"  ~ 1.04,
        treatment == "clearcutting" & forw_size == "medium" ~ 0.86,
        treatment == "clearcutting" & forw_size == "big"  ~ 0.73,
        treatment == "thinning" & forw_size == "small"  ~ 1.18,
        treatment == "thinning" & forw_size == "medium" ~ 0.67,
        treatment == "thinning" & forw_size == "big"  ~ 0.67,
        TRUE ~ 1.04),
      Vharv2 = dplyr::case_when( # Trunkating to model range
        treatment == "thinning" ~ trunc_vls(Vharv, 25, 125),
        TRUE ~  trunc_vls(Vharv, 50, 350) ), # clearcutting
      t4 = K1 * ((a + K2*Vharv2 + b*sqrt(Vharv2))/Vharv2 )
      ) %>%
     dplyr::select(., t4)



  # heureka t5 driving time forwarder (G15-min / m3fub) -----
    tt5 <- stands_df  %>%
      dplyr::select(., Y, L, treatment, forw_size, D) %>%
      dplyr::mutate(.,
        speed = (75 - (8.2 * Y) - (1.4 * L^2)), # Brunberg 04; meter / G15-min
        speed = dplyr::if_else(treatment == "clearcutting", speed , (0.85 * speed)),
        c_cap = dplyr::case_when( # Volume capacity, m3fub per load
          forw_size == "small" ~ 9.5,
          forw_size == "medium" ~ 13.6,
          forw_size == "large" ~ 17.9,
          TRUE ~ 17.9),
        t5 = (2*D / (speed * c_cap))) %>%
      dplyr::select(., t5)

  # heureka_t6 Assortment dependent time, G15min / m3fub -----
   tt6 <- stands_df %>%
     dplyr::select(., v) %>%
     dplyr::mutate(.,
                   vtr = trunc_vls(v, 0, 0.5),
                   t6 = 0.05 - vtr    # Brunberg 2004
       ) %>%
     dplyr::select(., t6)

  # heureka t7 forwarder sorting time, G15min / m3fub  ----
     tt7 <- stands_df  %>%
       dplyr::select(., nbAssortments ) %>%
       dplyr::mutate(., t7 = -0.1 + 0.1 * nbAssortments ) %>%
       dplyr::select(., t7)



  # heureka t8 forwarder additional time, G15 minutes per hectare ----

     # return forwarder additional time, G15 minutes per hectare
     #  assuming 1.5 minute per load, and calculating loads per hectare
     tt8 <- stands_df  %>%
     dplyr::select(., forw_size, Vharv) %>%
     dplyr::mutate(.,
             c_cap = dplyr::case_when(
               forw_size == "small" ~ 9.5,
               forw_size == "medium" ~ 13.6,
               forw_size == "large" ~ 17.9,
               TRUE ~ 17.9),
             nbLoads = floor((Vharv / c_cap) + .99),
             t8 = 1.5 * nbLoads) %>%
     dplyr::select(., t8)

  # merging forwarder times tt4 ... tt7 ------

   tforw_df = dplyr::bind_cols(tt4, tt5, tt6,  tt7) %>%
     dplyr::mutate(., t_forw.m3 = rowSums(.)) %>%
     dplyr::bind_cols(., stands_df, tt8) %>%
     dplyr::mutate(., forw_G15min.ha = t_forw.m3 * Vharv + t8,
            forw_G15min.m3 = forw_G15min.ha/Vharv,
            forw_G15h.ha = round((forw_G15min.ha / 60), 2),
            forw_m3.G15h = 60 / forw_G15min.m3) %>%
     dplyr::select(., -tidyselect::starts_with("tt"),
                      -tidyselect::num_range("t", 4:8)) %>%
     dplyr::select(., tidyselect::starts_with("forw_"), tidyselect::everything())

   return(tforw_df)
}




#' Time consumption harvester and forwarder
#'
#' @param stands_df A data frame of stands for which harvester and forwarder
#' time consumption is to be estimated.
#' Mandatory variables in stands_df:
#'   v mean volume m3fub of the harvested trees in the stand
#'   Nharv Number of trees per hectare
#'
#'  Optional variables, of which default values is set as first value:
#'   nbAssortments = 4; Number of qualities and species. Afect t7 ForwarderSortingTime
#'   treatment = one of "clearcutting"|"thinning"
#'   L = 2, slope class (1:5) where 5 is steep
#'   Y = 2, surface structure (1:5) where 5 is uneven
#'   D = 300, Terrain transport distance (one way average)
#'
#' @param forw_size can be "small", "medium", "large"
#'
#' Should also take "xl" and "xxl" but not yet implemented
#' @param modelversion one of "Brunberg" | "Talbot16"
#'
#' @return a data frame with harvester and forwarder time consumption (hrs hectare^-1)
#' @export
#'
#' @examples
#' harv_forw(stands_df = testdf_volume) %>% dplyr::glimpse()
#' harv_forw(stands_df = stand_data_norsk_prodnorm, forw_size = "large", modelversion = "Talbot16") %>% dplyr::glimpse()
#'
harv_forw <- function(stands_df, forw_size = "large", modelversion = "Talbot16"){

  Tharvdat <- t_harv(stands_df, modelversion = modelversion)
  Tforwdat <- t_forw(stands_df, forw_size = forw_size, modelversion = modelversion)
  Ttot <- dplyr::left_join(Tharvdat, Tforwdat) %>%
    dplyr::select(., tidyselect::starts_with("harv_"), tidyselect::starts_with("forw_"), tidyselect::everything())

  return(Ttot)
}


