


# Harvester time consumption model ----
#' t_harv_ccf_fj94
#'
#' @description
#' `t_harv_ccf_fj94()` estimates harvester time consumption for felling and processing trees using
#' Cut-to-length single grip harvester in selective and patch cutting, i.e. two continious cover approaches
#'
#' @details
#' The study was done using a Valmet 901, 83 kW and 11 tonne, 9.6m boom reach.
#' THe study was done on even surface, good to medium ground conditions,
#' flat terrain, with some boulders.
#' The stands were spruce dominated, site index G11 - G15,
#' stand density was 1800 - 2100 stems/ha, 163 - 308 m3/ha.
#'
#'
#' @param stands_df A data frame having stand characteristics to be used in the
#' May include all the parameters set in the function call except "verbose"
#' @param Stems_ha Number of stems per hectare,
#' @param v mean volume m3fub of the harvestable trees in the stand.
#' @param Stems_ha Number of stems per hectare,
#' @param v mean volume m3fub of the trees in the stand.
#' @param treatment one of :  "single_tree", "patch"
#' @param harvest_strength_pct  share of basal area to be cut. Range 0-100
#' @param patch_size_ha size (hectare) of each harvested patch in patch-based harvesting
#' @param sr_w strip road width in meter
#' @param striproad_exist TRUE/FALSE if striproad allready exist
#' @param L slope class (1:5) where  1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\%
#' @param Y surface structure (1:5) where 5 is uneven
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
#' t_harv_ccf_fj94(stands_df = testdata_heureka_thinning, treatment = "patch_harvest", harvest_strength_pct = 25, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_ccf_fj94(stands_df = testdata_heureka_clearcutting, treatment = "patch_harvest", harvest_strength_pct = 45,  verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_ccf_fj94(stands_df = testdata_heureka_clearcutting, treatment = "patch_harvest", harvest_strength_pct = 45, striproad_exist = TRUE, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_ccf_fj94(stands_df = testdata_heureka_clearcutting, treatment = "single_tree_selection", harvest_strength_pct = 45,  verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_ccf_fj94(stands_df = testdata_heureka_clearcutting, treatment = "single_tree_selection", harvest_strength_pct = 45, striproad_exist = TRUE,  verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_ccf_fj94(stands_df = testdata_fjeld,  treatment = "single_tree_selection", harvest_strength_pct = 45, verbose = TRUE) %>% dplyr::glimpse()
#' t_harv_ccf_fj94(stands_df = testdata_fjeld,  treatment = "patch_harvest", harvest_strength_pct = 45, verbose = TRUE) %>% dplyr::glimpse()
t_harv_ccf_fj94 = function(stands_df,
                          Stems_ha = 1200, v = 0.4,
                           treatment = c("patch_harvest", "single_tree_selection" )[1],
                           harvest_strength_pct = 45, #pct basal area
                           patch_size_ha = 0.25, # = 50x50 m
                           sr_w = 4 ,
                           striproad_exist = FALSE,
                           Y = 1, L = 1,
                           verbose = FALSE){

  # prepping data frame ----
  if(is.null(stands_df)){
    stands_df <- data.frame(Stems_ha = Stems_ha, v = v)
  }
  stopifnot(is.data.frame(stands_df))
  standvars <- colnames(stands_df)

  # prepping data frame ----
  # If variables are specified in df; keep them, if not; use as defined in function
  standvars <- colnames(stands_df)
  if(!('Stems_ha' %in% standvars)) { stands_df$Stems_ha <- Stems_ha}
  if(!('v' %in% standvars)) { stands_df$v <- v}
  if(!('sr_w' %in% standvars)) { stands_df$sr_w <- sr_w}
  if(!('harvest_strength_pct' %in% standvars)) { stands_df$harvest_strength_pct <- harvest_strength_pct}
  if(!('striproad_exist' %in% standvars)) { stands_df$striproad_exist <- striproad_exist}
  if(!('Y' %in% standvars)) { stands_df$Y <- Y}
  if(!('L' %in% standvars)) { stands_df$L <- L}


  stands_df$modelversion = "Fjeld94"
  stopifnot(treatment %in% c("patch_harvest", "single_tree_selection"))


    # Removal per hectare
  stands_df$Vharv = stands_df$Stems_ha * stands_df$v * stands_df$harvest_strength_pct/100



  if(treatment == "patch_harvest"){

    stands_df$patch_size_ha = trunk_rng(patch_size_ha, 100/10000 , 0.5)
    stands_df$ptch_w = sqrt(stands_df$patch_size_ha) *100; # widht and length (m) of patch in patch harvest
    # Striproad spacing in patch harvest
    stands_df$sr_sp =  (sqrt( stands_df$sr_w^2 - 4* stands_df$sr_w* (stands_df$harvest_strength_pct/100) * stands_df$ptch_w + 4 * (stands_df$harvest_strength_pct/100) * stands_df$ptch_w^2 ) + stands_df$sr_w) /
      (2 * (stands_df$harvest_strength_pct/100))


    # Striproad share in patch harvest, share of area to be harvested in the operation
    stands_df <- stands_df %>% dplyr::mutate(
      treatment = "patch_harvest",
      # Striproad area per hectare
      sr_share_harvest_area = (.data$sr_sp - .data$ptch_w) * .data$sr_w / (.data$harvest_strength_pct *  .data$sr_sp^2 / 100),
      sr_share_tot_area = (.data$sr_sp - .data$ptch_w) * .data$sr_w / (.data$sr_sp^2),

      # Striproad volume and trees
      Nharv_sr =
        dplyr::case_when(
          .data$striproad_exist == FALSE ~ sr_share_harvest_area * Stems_ha,
          TRUE ~ 0),
      Vharv_sr =
        dplyr::case_when(
          .data$striproad_exist == FALSE ~  sr_share_harvest_area * Stems_ha * v,
          TRUE ~ 0),
      v_sr =
        dplyr::case_when(
          .data$striproad_exist == FALSE ~  Vharv_sr / Nharv_sr,
          TRUE ~ 0)
      )


      # The so-called selection factor (kind of dummy variable)
      stands_df$sf_faktor = 1

      #Patch area volume and trees
      stands_df <- stands_df %>%
        dplyr::mutate( Vharv_bsr = .data$Vharv - .data$Vharv_sr,  # Between strip roads
                v_bsr = .data$v,
                Nharv_bsr =  .data$Vharv_bsr / .data$v_bsr,
                Nharv = (.data$Nharv_sr + .data$Nharv_bsr))



    } else if (treatment == "single_tree_selection"){




      stands_df$sf_faktor = 2 # The so-called selection factor (kind of dummy variable)
      stands_df$sr_sp = 22 # Assuming strip road spacing 22m.

      striproad_exist = FALSE
      stands_df <- stands_df %>%
        dplyr::mutate(
          treatment = "single_tree_selection",
          sr_share_harvest_area =  .data$sr_w / .data$sr_sp,
          sr_share_tot_area = .data$sr_w / .data$sr_sp, # same as area-based share
          Nharv_sr = dplyr::case_when(
            .data$striproad_exist == FALSE ~ .data$Stems_ha * .data$sr_share_harvest_area,
                 TRUE ~ 0 ),
         v_sr = dplyr::case_when(
                 .data$striproad_exist == FALSE ~ .data$v,
                 TRUE ~ 0 ),
         Vharv_sr =  .data$Stems_ha * .data$sr_share_harvest_area * .data$v_sr,
         Vharv_bsr = .data$Vharv - .data$Vharv_sr , # Between strip roads
         v_bsr = .data$v * 1.3, # selective trees larger than stand average, here assuming 30% larger stems. Ref: Dfjeld 1994 page 18 first section.
         Nharv_bsr =  .data$Vharv_bsr / .data$v_bsr,
         Nharv = round(.data$Nharv_sr + .data$Nharv_bsr, 0),
         Vharv = (.data$Vharv_sr +.data$Vharv_bsr),
         v_hrv = round(.data$Vharv / .data$Nharv, 3 ))
  }


#vprint(names(stands_df))
  # Dag Fjeld 1994, main study, cmin/m3, E0-tid  ----
 stands_df <- stands_df %>%
        dplyr::mutate(
          T_proc_cmin_m3 = 81.537 + (29.4662 / .data$v_bsr) + 31.120 * .data$sf_faktor,
          T_move_cmin_m3 = 5.756 + (539.574 / (.data$v_bsr * harvest_strength_pct )),
          T_move_cmin_m3_2 = 5.756 + (539.574 / (.data$v_bsr * harvest_strength_pct * (1 + (50/.data$Nharv) - 0.1*.data$Y - 0.1*.data$L))), # This is a pure guestimate of combining BB97 and FJ94
          T_clpr_cmin_m3 = 15.84,
          T_bsr_cmin_m3 = (.data$T_proc_cmin_m3 + .data$T_move_cmin_m3 + .data$T_clpr_cmin_m3),
          T_bsr_cmin_ha = (.data$T_proc_cmin_m3 + .data$T_move_cmin_m3 + .data$T_clpr_cmin_m3) * .data$Vharv_bsr ,
          T_bsr_cmin_m3_2 = (.data$T_proc_cmin_m3 + .data$T_move_cmin_m3_2 + .data$T_clpr_cmin_m3),
          T_bsr_cmin_ha_2 = (.data$T_proc_cmin_m3 + .data$T_move_cmin_m3_2 + .data$T_clpr_cmin_m3) * .data$Vharv_bsr ,

          T_sr_cmin_m3 = dplyr::case_when(
            .data$striproad_exist == FALSE ~ 81.537 + (29.4662/.data$v_sr) + 31.120 * 1.5 + 23.9 / .data$v_sr + 15.84,
            TRUE ~ NA_real_),
          T_sr_cmin_ha = dplyr::case_when(
            .data$striproad_exist == FALSE ~ .data$T_sr_cmin_m3 * .data$Vharv_sr,
            TRUE ~ 0),
          T_cmin_ha = .data$T_sr_cmin_ha + .data$T_bsr_cmin_ha,
          T_cmin_ha_2 = .data$T_sr_cmin_ha + .data$T_bsr_cmin_ha_2,

          T_mean_cmin_m3 = .data$T_cmin_ha / .data$Vharv,
          T_mean_cmin_m3_2 = .data$T_cmin_ha_2 / .data$Vharv


        )



  # wrap up ----
    stands_df <- stands_df %>%
    dplyr::mutate(cmin_tree = .data$T_mean_cmin_m3 * v, # Study was on E0-time
                  cmin_tree_2 = .data$T_mean_cmin_m3_2 * v, # Study was on E0-time
                  harv_G15min.tree = .data$cmin_tree * 1.5 / 100, # Study was on E0-time
                  harv_G15min.ha = .data$T_cmin_ha * 1.5 / 100, # Study was on E0-time
                  harv_G15h.ha = round(.data$harv_G15min.ha / 60, 3),
                  harv_G15min.m3 = round(.data$harv_G15min.ha / .data$Vharv, 3),
                  harv_m3.G15h = round(60 / .data$harv_G15min.m3, 3)) %>%
    dplyr::select( tidyselect::starts_with("harv_"), tidyselect::everything())


  if(verbose == FALSE){
    stands_df <- stands_df %>% dplyr::select(tidyselect::starts_with("harv_G"), "harv_m3.G15h")
  }
  return(stands_df)
}
