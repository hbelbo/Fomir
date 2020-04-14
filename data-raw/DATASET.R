## code to prepare `DATASET` dataset goes here


# Stand data to test harvester forwarder functions ----
# The following is used to test if our implementation get similar results as the
#    Heureka implementation, see Heureka documentation p13.
heureka_testdata_clearcutting <- data.frame(
  v = c(0.8, 0.4, 1.5, 0.7),
  Nharv = c(500, 600, 700, 500),
  Y = c(1,1,1,2), # Surface (Ytstruktur)
  L = c(1,1,2,2), # Slope (lutning), Definition according “SkogForsk terrängtypschema”
  # W = default, # width of strips between striproads
  treatment = c(rep("clearcutting", 4)),
  solution_hr = c(9.0519, 7.540, 20.685, 8.5607),
  stringsAsFactors = F
)


heureka_testdata_thinning <- data.frame(
  v =  c(0.1, 0.1, 0.3, 0.2),
  Nharv = c(500, 500, 500, 500),
  Nres = c( 1200, 1200, 1000, 1000),
  Y = c(1, 1, 1, 1),
  L = c(1, 1, 1, 1),
  W = 16, # width of strips between striproads
  p_init_spruce = c( 0.7, 0.7, 0.5, 0.5),
  Pharv_broadLeaves = c(.2, .2, 0, 0),
  rd = c(1, rep(1.1, 3)),
  thinningnumber = c( 2, 2, 3, 3),
  thinningsystem = c( rep("striproad", 4)),
  treatment = c(rep("thinning", 4)),
  solution_hr = c(5.159, 5.1266, 6.5545, 5.5903),
  stringsAsFactors = F
)


heureka_testdat_forwarding <- data.frame(
  v     =  c(1, 2, 0.2, 0.1, 0.2),
  Nharv = c(700, 700, 700, 600, 600),
  Y = c(1, 1, 1, 2, 2),
  L = c(1, 1, 1, 2, 2),
  D = c(300, 300, 500, 600, 600),
  treatment = c(rep("clearcutting", 2), rep("thinning", 3),
  nbAssortments = c(4, 4, 3, 2, 2)),
  solution_hr = c(21.76, 30.08, 12.266, 5.43, 9.39),
  stringsAsFactors = F
)




stand_data_norsk_prodnorm <- data.frame(
  v = c(0.25, rep(rep(0.4, 4), 2)),
  Nharv = c(1000, rep(rep(1000, 4), 2)),
  Nres = c(0, rep(c( 0,0,0,0), 2)),
  Y = c(2, rep(c(2, 4, 2, 4), 2)),
  L = c(2, rep(c(2, 2, 4, 4), 2)),
  D = c(700, c(rep(350, 4), rep(650, 4))),
  p_init_spruce = 1,
  Pharv_broadLeaves = 0,
  solution_hr = NA,
  stringsAsFactors = F
)

testdf_volume <- data.frame(
  v = seq(0.05, 2.05, length = 200),
  Nharv = rep(1000, 200),
  Nres = 0,
  Y = 2, L = 2, D = 700,
  stringsAsFactors = F)




usethis::use_data(
  heureka_testdata_clearcutting,
  heureka_testdat_thinning,
  heureka_testdat_forwarding,
  heureka_harv_forw_testdat,
  stand_data_norsk_prodnorm,
  testdf_volume,
  overwrite = T)
