
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Fomir

<!-- badges: start -->

<!-- badges: end -->

Forest operation models in R

The goal of Fomir is to provide functions for estimating time
consumption, costs, and other inputs in forest operations. Unless
otherwise specified, all functions take a data.frame as argument and
assume each row is one treatment unit, normally one forest stand.
Reference literature for all functions is provided in the “inst/extdata”
direcotry of the package.  
For the moment there are functions for

  - Harvesting: time consumption per per stem, per m^3, per ha
  - Forwarding: time consumption per m3 & per ha

## Installation

Installation of `Fomir` from github:

``` r
devtools::install_github("hbelbo/Fomir")
```

Load Fomir and dependent packages:

``` r
invisible( lapply( c("magrittr", "dplyr", "Fomir"), library, character.only = T))
```

## Harvesting and forwarding operations

Time consumption models for harvester and forwarder are based on
Brunberg’s time consumption models, found in the Heureka documentation
<https://www.heurekaslu.se/w/images/8/8c/Time_consumption_harvester_and_forwarder.pdf>
A Norwegian counterpart / adaption is found at Skogkurs
<https://www.skogkurs.no/kunnskapsskogen/artikkel.cfm?Id_art=3269>

### stands data and variables

Stand characteristics like mean stem volume, slope, species composition
etc must follow the naming conventions used in the Heureka documentation
below.  
NB: Reasonable default values for all variables except mean stem size
and harvested trees per ha are provided within the functions.

<img src="man/figures/Stand_Variables_p1.png" />
<img src="man/figures/Stand_Variables_p2.png" />

``` r
library(Fomir)

## example of data frame of stands to be clearcuted

testdata_heureka_clearcutting %>% dplyr::glimpse()
#> Observations: 4
#> Variables: 6
#> $ v           <dbl> 0.8, 0.4, 1.5, 0.7
#> $ Nharv       <dbl> 500, 600, 700, 500
#> $ Y           <dbl> 1, 1, 1, 2
#> $ L           <dbl> 1, 1, 2, 2
#> $ treatment   <chr> "clearcutting", "clearcutting", "clearcutting", "clearc...
#> $ solution_hr <dbl> 9.0519, 7.5400, 20.6850, 8.5607
```

``` r
## t_harv()
t_harv_clearfell_bb(stands_df = testdata_heureka_clearcutting, verbose = TRUE) %>% dplyr::glimpse()
$ harv_G15min.tree <dbl> 1.0862327, 0.7540071, 1.7730184, 1.0272884
$ harv_G15min.ha   <dbl> 543.1164, 452.4042, 1241.1129, 513.6442
$ harv_G15h.ha     <dbl> 9.052, 7.540, 20.685, 8.561
$ harv_G15min.m3   <dbl> 1.358, 1.885, 1.182, 1.468
$ harv_m3.G15h     <dbl> 44.183, 31.830, 50.761, 40.872
$ harv_stems.G15h  <dbl> 55.2, 79.6, 33.8, 58.4
$ t1               <dbl> 6.451134, 5.477378, 5.375945, 8.294315
$ t2               <dbl> 75.50523, 50.92317, 129.41009, 69.12787
$ t3               <dbl> 1.6, 1.6, 1.6, 1.6
$ ttot_cmin_tree   <dbl> 83.55636, 58.00054, 136.38603, 79.02219
$ v                <dbl> 0.8, 0.4, 1.5, 0.7
$ Stems_ha         <dbl> 500, 600, 700, 500
$ Nharv            <dbl> 500, 600, 700, 500
$ Y                <dbl> 1, 1, 1, 2
$ L                <dbl> 1, 1, 2, 2
$ solution_hr_ha   <dbl> 9.0519, 7.5400, 20.6850, 8.5607
$ modelversion     <chr> "Brunberg95", "Brunberg95", "Brunberg95", "Brunberg95"
$ Nres             <dbl> 0, 0, 0, 0
$ V.init           <dbl> 400, 240, 1050, 350
$ v_hrv            <dbl> 0.8, 0.4, 1.5, 0.7
$ Vharv            <dbl> 400, 240, 1050, 350
$ treatment        <chr> "clearcutting", "clearcutting", "clearcutting", "clearcutting"
$ Nharv_tr         <dbl> 500, 600, 700, 500
$ K                <dbl> 25.9, 25.9, 25.9, 25.9
$ S                <dbl> 13.3, 13.3, 13.3, 13.3
$ phindrance       <dbl> 0.021030328, 0.008042079, 0.094129497, 0.016599056
$ pdoublesawed     <dbl> 0.05732418, 0.01477403, 0.41338242, 0.04109128
$ pdifficult       <dbl> 0.04012692, 0.01861790, 0.13847128, 0.03319811
$ p                <dbl> 0, 0, 0, 0
$ cc_intc          <dbl> 27.3, 27.3, 27.3, 27.3
$ cc_slp           <dbl> 56, 56, 56, 56


## t_forw_bb()
t_forw_bb(stands_df = testdata_heureka_forwarding, verbose = TRUE) %>% dplyr::glimpse()
Rows: 5
Columns: 33
Rowwise: 
$ forw_size            <chr> "big", "big", "small", "medium", "medium"
$ forw_G15min.ha       <dbl> 1264.5917, 2527.6835, 735.9581, 325.8370, 563.6533
$ forw_G15min.m3       <dbl> 1.8, 1.8, 5.3, 5.4, 4.7
$ forw_G15h.ha         <dbl> 21.08, 42.13, 12.27, 5.43, 9.39
$ forw_m3.G15h         <dbl> 33.21, 33.23, 11.41, 11.05, 12.77
$ v                    <dbl> 1.0, 2.0, 0.2, 0.1, 0.2
$ Stems_ha             <dbl> 700, 700, 700, 600, 600
$ Y                    <dbl> 1, 1, 1, 2, 2
$ L                    <dbl> 1, 1, 1, 2, 2
$ D                    <dbl> 300, 300, 500, 600, 600
$ treatment            <chr> "clearcutting", "clearcutting", "thinning", "thinning", "thinning"
$ nbAssortments        <dbl> 4, 4, 3, 2, 2
$ solution_hr_ha       <dbl> 21.760, 30.080, 12.266, 5.430, 9.390
$ modelversion         <chr> "Brunberg", "Brunberg", "Brunberg", "Brunberg", "Brunberg"
$ D_basveg             <dbl> 200, 200, 200, 200, 200
$ harvest_strength_pct <dbl> 100, 100, 100, 100, 100
$ Vharv                <dbl> 700, 1400, 140, 60, 120
$ Nharv                <dbl> 700, 700, 700, 600, 600
$ a                    <dbl> 5.7, 5.7, -43.0, -43.0, -43.0
$ b                    <dbl> 11.45, 11.45, 25.90, 25.90, 25.90
$ K1                   <dbl> 1, 1, 1, 1, 1
$ K2                   <dbl> 0.73, 0.73, 1.18, 0.67, 0.67
$ Vharv2               <dbl> 350, 350, 125, 60, 120
$ t4                   <dbl> 1.358314, 1.358314, 3.152566, 3.297009, 2.676002
$ speed                <dbl> 65.40, 65.40, 55.59, 45.05, 45.05
$ c_cap                <dbl> 17.9, 17.9, 9.5, 13.6, 13.6
$ t5                   <dbl> 0.5125314, 0.5125314, 1.8935628, 1.9586081, 1.9586081
$ vtr                  <dbl> 0.5, 0.5, 0.2, 0.1, 0.2
$ t6                   <dbl> -0.45, -0.45, -0.15, -0.05, -0.15
$ t7                   <dbl> 0.3, 0.3, 0.2, 0.1, 0.1
$ nbLoads              <dbl> 40, 79, 15, 5, 9
$ t8                   <dbl> 60.0, 118.5, 22.5, 7.5, 13.5
$ t4t7                 <dbl> 1.720845, 1.720845, 5.096129, 5.305617, 4.584610


##bb_harv_forw(stands_df = testdata_heureka_forwarding) %>% dplyr::glimpse()
Rows: 5
Columns: 9
$ harv_G15min.tree <dbl> 1.2397905, 2.3590528, 0.5928738, 0.5481711, 0.6230530
$ harv_G15min.ha   <dbl> 867.8534, 1651.3369, 415.0116, 328.9026, 373.8318
$ harv_G15h.ha     <dbl> 14.464, 27.522, 6.917, 5.482, 6.231
$ harv_G15min.m3   <dbl> 1.240, 1.180, 2.964, 5.482, 3.115
$ harv_m3.G15h     <dbl> 48.387, 50.847, 20.243, 10.945, 19.262
$ forw_G15min.ha   <dbl> 1264.5917, 2527.6835, 735.9581, 325.8370, 563.6533
$ forw_G15min.m3   <dbl> 1.8, 1.8, 5.3, 5.4, 4.7
$ forw_G15h.ha     <dbl> 21.08, 42.13, 12.27, 5.43, 9.39
$ forw_m3.G15h     <dbl> 33.21, 33.23, 11.41, 11.05, 12.77
```
