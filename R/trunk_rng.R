# trunk_rng ---------
#' trunk_rng
#'
#' @param x numeric vector to be trunkated
#' @param min_val minimum value, x lower than min_val will be replaced with min_val
#' @param max_val max value, x larger than min_val will be replaced with max_val
#'
#' @return x where extreme values ar truncated to within min_val and max_val
#' @export
#'
#' @examples trunk_rng(1:10, 3, 6)
trunk_rng <- function(x, min_val, max_val){
  x[x > max_val] <- max_val
  x[x < min_val] <- min_val
  return(x)
}

