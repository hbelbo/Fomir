#' Test data set for Brunberg's clearcutting model
#'
#' @format
#' data.frame() with the following content:
#' \describe{
#'  \item{v}{numeric; mean stem volume m3fub}
#'  \item{Nharv}{numeric; Number of harvested stems per hectare}
#'  \item{Y}{numeric; Surface structure (sv: Ytstruktur)}
#'  \item{L}{numeric; Slope class (sv: Lutning), Definition according “SkogForsk
#'  terrängtypschema” where 1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\% }
#'  \item{solution_hr}{numeric; expected model output, to check if Brunberg's
#'  clearcutting  model is doing correct estimates on the dataset}
#' }
"testdata_heureka_clearcutting"




#' Test data set for Brunberg's thinning model
#'
#' @format
#' data.frame() with the following content:
#' \describe{
#'  \item{v}{numeric; mean stem volume m3fub}
#'  \item{Nharv}{numeric; Number of harvested stems per hectare}
#'  \item{Y}{numeric; Surface structure (sv: Ytstruktur)}
#'  \item{L}{numeric; Slope class (sv: Lutning), Definition according “SkogForsk
#'  terrängtypschema” where 1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\% }
#'  \item{w}{width of strips between striproads}
#'  \item{p_init_spruce}{numeric; proportion of spruce before harvest}3
#'  \item{Pharv_broadLeaves}{proportion broadleaved trees of harvested trees}
#'  \item{rd}{relative diameter in thinning, i.e.
#'  mean dia harvested / mean dia residual trees. BA weighted mean diameters.}
#'  \item{thinningnumber}{one of: 1, 2}
#'  \item{thinningsystem}{one of: striproad, striproad_with_midfield_machine, striproad_with_midfield_chainsaw}
#'  \item{solution_hr}{numeric; expected model output, to check if Brunberg's
#'  thinning  model is doing correct estimates on the dataset}
#'  \item{D}{Terrain transport distance (one way average)}
#'  \item{modelversion}{text; is set to "Brunberg", alternative is "Talbot16}
#' }
"testdata_heureka_thinning"




#' Test data set for Brunberg's forwarder model
#'
#' @format
#' data.frame() with the following content:
#' \describe{
#'  \item{v}{numeric; mean stem volume m3fub}
#'  \item{Nharv}{numeric; Number of harvested stems per hectare}
#'  \item{Y}{numeric; Surface structure (sv: Ytstruktur)}
#'  \item{L}{numeric; Slope class (sv: Lutning), Definition according “SkogForsk
#'  terrängtypschema” where 1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\% }
#'  \item{D}{Terrain transport distance (one way average)}
#'  \item{nbAssortments}{numeric; Number of qualities and species. Afect t7 ForwarderSortingTime}
#'  \item{w}{width of strips between striproads}
#'  \item{forw_size}{can be "small", "medium", "large"}
#'  \item{modelversion}{text; is set to "Brunberg"  alternative is "Talbot16}
#'  \item{solution_hr}{numeric; expected model output, to check if Brunberg's
#'  thinning  model is doing correct estimates on the dataset}
#' }
"testdata_heureka_forwarding"






#' Test data set for varying volume and terrain
#'
#' @format
#' data.frame() with the following content:
#' \describe{
#'  \item{v}{numeric; mean stem volume m3fub}
#'  \item{Stems_ha}{Number of stems per hectare}
#'  \item{Nharv}{numeric; Number of harvested stems per hectare}
#'  \item{Y}{numeric; Surface structure (sv: Ytstruktur)}
#'  \item{L}{numeric; Slope class (sv: Lutning), Definition according “SkogForsk
#'  terrängtypschema” where 1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\% }
#'  \item{D}{Terrain transport distance (one way average)}
#' }
"testdata_clearcut_forw"







#' Test data set for varying volume and terrain
#'
#' @format
#' data.frame() with the following content:
#' \describe{
#'  \item{v}{numeric; mean stem volume m3fub}
#'  \item{Stems_ha}{Number of stems per hectare}
#'  \item{Nharv}{numeric; Number of harvested stems per hectare}
#'  \item{Y}{numeric; Surface structure (sv: Ytstruktur)}
#'  \item{L}{numeric; Slope class (sv: Lutning), Definition according “SkogForsk
#'  terrängtypschema” where 1 is flat (0-10\%), 2 is 10-20\%, 3 is 20-33\%, 4 is 33-50\%, 5 is >50\% }
#'  \item{D}{Terrain transport distance (one way average)}
#' }
"testdata_sensblt_v_Y_L_D"





#' Test data set for testing Fjeld 94 bledningshogst
#'
#' @format
#' data.frame() with the following content:
#' \describe{
#'  \item{v}{numeric; mean stem volume m3fub}
#'  \item{Stems_ha}{Number of stems per hectare}
#' }
"testdata_fjeld"




