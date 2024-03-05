## code to prepare `DATASET` dataset goes here


# Stand data to test harvester forwarder functions ----
# The following is used to test if our implementation get similar results as the
#    Heureka implementation, see Heureka documentation p13.
testdata_heureka_clearcutting <- data.frame(
  v = c(0.8, 0.4, 1.5, 0.7),
  Stems_ha =  c(500, 600, 700, 500),
  Nharv = c(500, 600, 700, 500),
  Y = c(1,1,1,2), # Surface (Ytstruktur)
  L = c(1,1,2,2), # Slope (lutning), Definition according “SkogForsk terrängtypschema”   where  1 is flat (0-10%), 2 is 10-20%, 3 is 20-33%, 4 is 33-50%, 5 is >50%
  # W = default, # width of strips between striproads
  solution_hr_ha = c(9.0519, 7.540, 20.685, 8.5607), # If the model script gives these results, the model is correctly put together
  stringsAsFactors = F
)


Nharv = c(500, 500, 500, 500)
Nres = c( 1200, 1200, 1000, 1000)
Stems_ha = Nharv +Nres
testdata_heureka_thinning <- data.frame(
  v =  c(0.1, 0.1, 0.3, 0.2),
  Stems_ha = Stems_ha,
  Nharv = Nharv,
  Y = c(1, 1, 1, 1),
  L = c(1, 1, 1, 1),
  W = 16, # width of strips between striproads
  p_init_spruce = c( 0.7, 0.7, 0.5, 0.5),
  Pharv_broadLeaves = c(.2, .2, 0, 0),
  rd = c(1, rep(1.1, 3)),
  thinningnumber = c( 2, 2, 3, 3),
  thinningsystem = c( rep("striproad", 4)),
  treatment = c(rep("thinning", 4)),
  solution_hr_ha = c(5.159, 5.1266, 6.5545, 5.5903),  # If the model script gives these results, the model is correctly put together
  D = 200,
  modelversion = "Brunberg97",
  stringsAsFactors = F
)



testdata_heureka_forwarding <- data.frame(
  v     =  c(1, 2, 0.2, 0.1, 0.2),

  Stems_ha = c(700, 700, 700, 600, 600),
  Y = c(1, 1, 1, 2, 2),
  L = c(1, 1, 1, 2, 2),
  D = c(300, 300, 500, 600, 600),
  treatment = c(rep("clearcutting", 2), rep("thinning", 3)),
  nbAssortments = c(4, 4, 3, 2, 2),
  solution_hr_ha = c(21.76, 30.08, 12.266, 5.43, 9.39),  # If the model script gives these results, the model is correctly put together
  forw_size  = c("big", "big", "small", "medium", "medium"),
  modelversion = "Brunberg",
  stringsAsFactors = F
)




testdata_clearcut_forw <- data.frame(
  expand.grid(v= seq(0.05, 2.05, length = 200),
              Stems_ha = 1000,
              Nharv = 1000,
              Y = c(2, 4),
              L = c(2, 4),
              D = c(350, 650, 700),
              stringsAsFactors = F)
)

testdata_sensblt_v_Y_L_D <- data.frame(
  expand.grid(v= seq(0.05, 2.05, length = 200),
              Stems_ha = 1000,
              Nharv = 1000,
              Y = c(2, 4),
              L = c(2, 4),
              D = c(350, 700, 1400, 2100),
              stringsAsFactors = F)
  )




testdata_fjeld <- expand.grid(v = seq(1/6, 1/2, length = 5),
                              Stems_ha = 1000)



usethis::use_data(
  testdata_heureka_clearcutting,
  testdata_heureka_thinning,
  testdata_heureka_forwarding,
  testdata_clearcut_forw,
  testdata_sensblt_v_Y_L_D,
  testdata_fjeld,
  overwrite = T)
