

#' Logging residues forwarder model
#' Time consumption model: Nurmi 2007, Recovery of LR for energy from spruce stands.
#' Fuel consumption model: Brunberg 2012, Bränsleförbrukningen hos skogsmaskiner 2012.
#'
#' @param species numeric code: 1 = gran, 2 = furu, 3 = lauv, 4 = andre
#' @param ForwD_km numeric: forwarding distance (one way)
#'
#' @return single number, the forwarder cost (NOK) per m3solid logging residues
#' @export
#'
#' @examples lr_forw(species = 1, ForwD_km = 0.4)
lr_forw <- function(species, ForwD_km) {
  #"species code: 1 = gran, 2 = furu, 3 = lauv, 4 = andre
  BD <- 0.5 # if spruce Basic density = 0.5 t/m3
  if (species == 2){
    BD <- 0.4
  } else if (species == 3) {
    BD <- 0.5
  }

  ## Time consumption estimate
  # Parameters
  d <- ForwD_km;
  # Other assumptions
  ObjSize <- 300; # Object size logging residues is 300 m3sob
  Obj_clustering <- 3; # Three cut objects per machine relocations
  CompanyMargin <- 10/100; # Assumed profit level

  # Time consumption models, t= productive machine hours (PMh15) per dry tonne or m3.
  # Original model: y = 18.602 + 0.1386*10^-2 * x - 7.705 * log(z) where y=tonnes/ PMh0-hour, x is load size (kg), z = forwarding distance (m).
  # ref original model: Nurmi 2007, Recovery of LR for energy from spruce stands.  http://dx.doi.org/10.1016/j.biombioe.2007.01.011
  x <- 9000; # kg load size
  P_t <- 18.602 + 0.1386*10^-2 * x - (7.705*log10(d*1000)); # Productivity, tonnes PMh0^-1
  t_t <- 1/P_t; # PMh0 per tonne
  t_td <- t_t * 1.084 / 0.45; # 1.084 to change from PMh0 to PMh15. Moisture content was 55# in the Nurmi study; therefore 0.45 to convert to dry matter basis.
  if (species != 1) {
    t_td <- t_td*1.1; # GUESTIMATE: Less biomass density in pine stands ~ 10% more time consumption per tonne
  }
  t_m3sob <- t_td * BD;

  # Available machine time:
  A_SMh <- 2300; #Annual scheduled production time for one harvester
  MU <- 0.8; #Machine utilization, i.e. A.pmh15 / A_SMh
  A_PMh15 <- A_SMh * MU; # Annual productive machine hours

  ## Annual production
  A_Prod_m3sob <- A_PMh15 / t_m3sob; # Annual  production, m3
  A_Prod_td <- A_PMh15 / t_td; # Annual  production, td


  # Annual number of cut objects
  cut_objects <- A_Prod_m3sob / ObjSize;

  ## Fuel consumption estimate
  # partial estimates, FC <- FuelConsumption
  # Case roundwood
  # Ref: Brunberg 2012, Bränsleförbrukningen hos skogsmaskiner 2012.  Arbetsrapport Från Skogforsk nr. 789-2013. http://www.skogforsk.se/PageFiles/73982/Br%C3%A4nslef%C3%B6rbrukningen%20hos%20skogsmaskiner%202012%20-%20Arbetsrapport%20789-2013.pdf
  # Original model roundwood forwarding
  FC_m3sub <- 0.53 + 0.00046 * d*1000; # Final harvest, equation to figure 5 in the report.
  #FC_m3sub <- 0.80 + 0.00039 * d*1000; # Thinning, equation to figure 6 in the report.
  FC_m3sob <- FC_m3sub / 1.1; # Assuming 10# bark
  FC_td <- FC_m3sob / BD;

  FC_td <- (FC_td * 1.1); # Guestimate LR forwarding 10% extra fuel constumption.
  FC_m3sob <- FC_td * BD;

  FC_OPt_day <- 5*40/100 ; # Operator transport, 40 km per day, 5 litre / 100 km
  FC_OPt_year <- FC_OPt_day * A_SMh / 8; # Assuming 8 h average shift length.
  FC_MachTrans_object <- (5.1+6.4)*20; # Machine transport ( by truck), assuming 20 km one way dist, 5.1 litres/km empty truck and 6.4 litres/km loaded truck
  FC_MachTrans_year <- FC_MachTrans_object* cut_objects / Obj_clustering; # Assuming clustering of 3 cut objects per machine relocation.

  # Fuel cons per year
  Cons_Diesel_machine <- FC_td *A_Prod_td ; # Annual fuel consumption of the machine
  Cons_Diesel_AUX <-  FC_OPt_year + FC_MachTrans_year; # Adding FC for auxiliary activities (operator transport, machine relocation)

  ## Other commodities consumption
  HydrOilCons <- 200*  A_PMh15/1000;  # 1000 h service interval Hydraulic oil
  EngineOilCon <- 50 * A_PMh15/500; # 500 h service interval for engine oil
  GearOilCons <- 200*  A_PMh15/2000; # 1000 h service interval Hydraulic oil
  TyreCons <- (4*200 + 2*270) * A_PMh15 / 10000 ;# boogie wheels 200 kg, front wheels 270 kg, wheel duration 10000 PMh15

  ## Cost estimate
  # CAPEX part, all estimates made in an spreadsheet machine cost calculator based on assumptions described in the "building blocks forest operations.doc"
  Purchase <- 3800000; # Purchase cost new machine
  CAPEX_Depr <- 400384*A_SMh / 1800; #400' based on linear depreciation and 1800SMh per year.
  CAPEX_Rate <- 114878*A_SMh / 1800;
  CAPEX_Rest <- 42000*A_SMh / 1800; # Machine insurance, fees
  CAPEX1 <- sum(c(CAPEX_Depr, CAPEX_Rate, CAPEX_Rest));
  CAPEX_Margin <- CAPEX1 * CompanyMargin;
  CAPEX <- sum(c(CAPEX1, CAPEX_Margin));

  # OPEX part
  Anleggsdiesel <- 9.608; # kr per liter uten mva

  OPEX_Fuel <- Cons_Diesel_machine * Anleggsdiesel;# Auxiliary fuel costs are paid through reimbursement to operator, machine truck etc.
  OPEX_Lub <- OPEX_Fuel*0.13; # Tumb rule for cost of lubricates for forwarders
  OPEX_Rep <- Purchase * A_PMh15 / 15000; #Assuming repair cost will equal purchase cost at 15000 PMh15.
  OPEX_DriveChain <- 13000 * A_PMh15 / 2000;
  OPEX_Tires <- 5000 * A_PMh15 / 2000;
  OPEX_OtherGear <- 40000 * A_PMh15 / 2000;
  OPEX_Reloc <- 4000 * A_PMh15 / 2000; #
  OPEX_Operator <- 271*A_SMh; # Includes gross salary, employer tax, OTP, injury insurance and 10%overhead.
  OPEX1 <- sum(c(OPEX_Fuel, OPEX_Lub, OPEX_Rep, OPEX_DriveChain, OPEX_Tires, OPEX_OtherGear, OPEX_Reloc, OPEX_Operator));
  OPEX_Margin <- OPEX1 * CompanyMargin;
  OPEX <- sum(c(OPEX1, OPEX_Margin));
  Totex <- sum(c(CAPEX,OPEX));
  CTotptd <- Totex/A_Prod_td;
  CTotpm3 <- Totex/A_Prod_m3sob;

  return(CTotpm3)
}




#' Logging residues forwarder simplified timecons model
#' Time consumption model: Nurmi 2007, Recovery of LR for energy from spruce stands.
#' Fuel consumption model: Brunberg 2012, Bränsleförbrukningen hos skogsmaskiner 2012.
#'
#' @param species species code: 1 = gran, 2 = furu, 3 = lauv, 4 = andre
#' @param ForwD_km numeric: forwarding distance (one way)
#'
#' @return single number, the forwarder cost (NOK) per m3solid logging residues
#' @export
#'
#' @examples lr_forw(species = 1, ForwD_km = 0.4)
lr_forw_simple_tc <- function(species, ForwD_km) {
  #"species code: 1 = gran, 2 = furu, 3 = lauv, 4 = andre
  BD <- 0.5 # if spruce Basic density = 0.5 t/m3
  if (species == 2){
    BD <- 0.4
  } else if (species == 3) {
    BD <- 0.5
  }

  ## Time consumption estimate
  # Parameters
  d <- ForwD_km;
  # Other assumptions
  ObjSize <- 300; # Object size logging residues is 300 m3sob
  Obj_clustering <- 3; # Three cut objects per machine relocations
  CompanyMargin <- 10/100; # Assumed profit level

  # Time consumption models, t= productive machine hours (PMh15) per dry tonne or m3.
  # Original model: y = 18.602 + 0.1386*10^-2 * x - 7.705 * log(z) where y=tonnes/ PMh0-hour, x is load size (kg), z = forwarding distance (m).
  # ref original model: Nurmi 2007, Recovery of LR for energy from spruce stands.  http://dx.doi.org/10.1016/j.biombioe.2007.01.011
  # assuming 9 tonne load size
  P_t <- 12.474 - (7.705*log10(d*1000)); # Productivity, tonnes PMh0^-1
  t_t <- 1/P_t; # PMh0 per tonne
  t_td <- t_t * 1.084 / 0.45; # 1.084 to change from PMh0 to PMh15. Moisture content was 55# in the Nurmi study; therefore 0.45 to convert to dry matter basis.
  if (species != 1) {
    t_td <- t_td*1.1; # GUESTIMATE: Less biomass density in pine stands ~ 10% more time consumption per tonne
  }
  t_m3sob <- t_td * BD;

  # Available machine time:
  A_SMh <- 2300; #Annual scheduled production time for one harvester
  MU <- 0.8; #Machine utilization, i.e. A.pmh15 / A_SMh
  A_PMh15 <- A_SMh * MU; # Annual productive machine hours

  ## Annual production
  A_Prod_m3sob <- A_PMh15 / t_m3sob; # Annual  production, m3
  A_Prod_td <- A_PMh15 / t_td; # Annual  production, td


  # Annual number of cut objects
  cut_objects <- A_Prod_m3sob / ObjSize;

  ## Fuel consumption estimate
  # partial estimates, FC <- FuelConsumption
  # Case roundwood
  # Ref: Brunberg 2012, Bränsleförbrukningen hos skogsmaskiner 2012.  Arbetsrapport Från Skogforsk nr. 789-2013. http://www.skogforsk.se/PageFiles/73982/Br%C3%A4nslef%C3%B6rbrukningen%20hos%20skogsmaskiner%202012%20-%20Arbetsrapport%20789-2013.pdf
  # Original model roundwood forwarding
  FC_m3sub <- 0.53 + 0.00046 * d*1000; # Final harvest, equation to figure 5 in the report.
  #FC_m3sub <- 0.80 + 0.00039 * d*1000; # Thinning, equation to figure 6 in the report.
  FC_m3sob <- FC_m3sub / 1.1; # Assuming 10# bark
  FC_td <- FC_m3sob / BD;

  FC_td <- (FC_td * 1.1); # Guestimate LR forwarding 10% extra fuel constumption.
  FC_m3sob <- FC_td * BD;

  FC_OPt_day <- 5*40/100 ; # Operator transport, 40 km per day, 5 litre / 100 km
  FC_OPt_year <- FC_OPt_day * A_SMh / 8; # Assuming 8 h average shift length.
  FC_MachTrans_object <- (5.1+6.4)*20; # Machine transport ( by truck), assuming 20 km one way dist, 5.1 litres/km empty truck and 6.4 litres/km loaded truck
  FC_MachTrans_year <- FC_MachTrans_object* cut_objects / Obj_clustering; # Assuming clustering of 3 cut objects per machine relocation.

  # Fuel cons per year
  Cons_Diesel_machine <- FC_td *A_Prod_td ; # Annual fuel consumption of the machine
  Cons_Diesel_AUX <-  FC_OPt_year + FC_MachTrans_year; # Adding FC for auxiliary activities (operator transport, machine relocation)

  ## Other commodities consumption
  HydrOilCons <- 200*  A_PMh15/1000;  # 1000 h service interval Hydraulic oil
  EngineOilCon <- 50 * A_PMh15/500; # 500 h service interval for engine oil
  GearOilCons <- 200*  A_PMh15/2000; # 1000 h service interval Hydraulic oil
  TyreCons <- (4*200 + 2*270) * A_PMh15 / 10000 ;# boogie wheels 200 kg, front wheels 270 kg, wheel duration 10000 PMh15

  ## Cost estimate
  # CAPEX part, all estimates made in an spreadsheet machine cost calculator based on assumptions described in the "building blocks forest operations.doc"
  Purchase <- 3800000; # Purchase cost new machine
  CAPEX_Depr <- 400384*A_SMh / 1800; #400' based on linear depreciation and 1800SMh per year.
  CAPEX_Rate <- 114878*A_SMh / 1800;
  CAPEX_Rest <- 42000*A_SMh / 1800; # Machine insurance, fees
  CAPEX1 <- sum(c(CAPEX_Depr, CAPEX_Rate, CAPEX_Rest));
  CAPEX_Margin <- CAPEX1 * CompanyMargin;
  CAPEX <- sum(c(CAPEX1, CAPEX_Margin));

  # OPEX part
  Anleggsdiesel <- 9.608; # kr per liter uten mva

  OPEX_Fuel <- Cons_Diesel_machine * Anleggsdiesel;# Auxiliary fuel costs are paid through reimbursement to operator, machine truck etc.
  OPEX_Lub <- OPEX_Fuel*0.13; # Tumb rule for cost of lubricates for forwarders
  OPEX_Rep <- Purchase * A_PMh15 / 15000; #Assuming repair cost will equal purchase cost at 15000 PMh15.
  OPEX_DriveChain <- 13000 * A_PMh15 / 2000;
  OPEX_Tires <- 5000 * A_PMh15 / 2000;
  OPEX_OtherGear <- 40000 * A_PMh15 / 2000;
  OPEX_Reloc <- 4000 * A_PMh15 / 2000; #
  OPEX_Operator <- 271*A_SMh; # Includes gross salary, employer tax, OTP, injury insurance and 10%overhead.
  OPEX1 <- sum(c(OPEX_Fuel, OPEX_Lub, OPEX_Rep, OPEX_DriveChain, OPEX_Tires, OPEX_OtherGear, OPEX_Reloc, OPEX_Operator));
  OPEX_Margin <- OPEX1 * CompanyMargin;
  OPEX <- sum(c(OPEX1, OPEX_Margin));
  Totex <- sum(c(CAPEX,OPEX));
  CTotptd <- Totex/A_Prod_td;
  CTotpm3 <- Totex/A_Prod_m3sob;

  return(CTotpm3)
}
