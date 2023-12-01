


# Time consumption harvester and forwarder
#' bb_harv_forw
#'
#' @param stands_df A data frame of stands for which harvester and forwarder
#' time consumption is to be estimated.
#'
#'

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
#' @return a data frame with harvester and forwarder time consumption estimates
#' \strong{variables in output df:}
#' \describe{#'
#' \item{harv_G15h.ha}{Harvester G15 hours per hectare}
#' \item{forw_G15h.ha}{Forwarder G15 hours per hectare}}
#' @details
#' \strong{variables in stands_df:}
#' \describe{
#' \item{v}{mean m3fub of harvested trees}
#' \item{Nharv}{N harvested trees per hectare}}
#'
#' @export
#'
#' @examples
#' bb_harv_forw(stands_df = testdata_clearcut_Talbot16) %>% dplyr::glimpse()
#' bb_harv_forw(stands_df = testdata_heureka_forwarding) %>% dplyr::glimpse()
bb_harv_forw <- function(stands_df, forw_size = "large", modelversion = "Talbot16"){

  Tharvdat <- t_harv_bb(stands_df, modelversion = modelversion)
  Tforwdat <- t_forw_bb(stands_df, forw_size = forw_size, modelversion = modelversion)
  Ttot <- dplyr::bind_cols(Tharvdat, Tforwdat) %>%
    dplyr::select( tidyselect::starts_with("harv_"), tidyselect::starts_with("forw_"), tidyselect::everything())

  return(Ttot)
}


