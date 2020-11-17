
RoadsideChipper = function(MainSpecies=1, Assortment=3) {
  # Wood characteristics
BD = 0.4; #Pine basic density is 400 kgd biomass per m3sob.
PStg = 0.05; # Piece size green tonne, 50 kg.  
if (MainSpecies == 1) { #"Spruce"
  
  BD = 0.38;   # 380 kgd biomass per m3sob
  PStg = 0.08; # Assuming 80 kg logs	
  if (Assortment == 1){ #"RoundWood" 
    BD = 0.38;   # 380 kgd biomass per m3sob
    PStg = 0.08; # Assuming 80 kg logs
  } else if (Assortment == 2){  #"LR" # If logging residues
    BD = 0.45; 
    PStg = 0.04; # Assuming average mass of tops (typically 95 kg) and slash (17 kg) is 40 kg. 
  } else if (Assortment == 3){ # Whole trees
    BD = 0.38;   # 380 kgd biomass per m3sob
    PStg = 0.08; # Assuming 80 kg logs	
  }


} else if (MainSpecies == 2){ # "Pine" # if pine
  BD = 0.4;   # 400 kgd biomass per m3sob
  PStg = 0.07; # Assuming 70 kg logs	
  if (Assortment == 1) { # Roundwood
    BD = 0.4;
    PStg = 0.08;
  } else if (Assortment == 2){ # "LR"
    BD = 0.4; 
    PStg = 0.03; 
  } else if (Assortment == 3) { # Whole trees
    BD = 0.4; 
    PStg = 0.05;
  }
 
} else if (MainSpecies == 3){ # if birch
    BD = 0.5; # 500 kgd biomass per m3sob
    PStg = 0.9; # Assuming 90 kg piece size
}

## Annual productivity estimate
# Original productivity model: Spinelli etal 2001, BiomassBioenergy 21 (2001) p 433-444. 
# t chipping (min / green tonne) = 0.02+ 13.1/(Piece Size (green tonne) * Power(kW)) + 566/ Power(kW). Pure chipping time
# t repositioning (min / green tonne) = 0.584 + 0.00744*(InStand_dummy / Piece size (gt)) - 0.385 *(Self-propelled_dummy). Repositioning on the terminal
# t other (min / green tonne) = 0.2 + 0.0157 * PieceSize (GT) + 4.72 * Small_Chipper_forwarderDummy. Other denotes repositioning the chip receptacle under the chip spout. 
# For chipper-forwarders it includes using the chipper as a forwarder, i.e. forwarding chipped material from cutting site to chip landing. 
# Note: this is effective chipping time, add time for relocation, chip-bin exchange, etc 
# Note2: The study was on green wood (not dried), i.e. MC 40-55#. We assume productivity on dry matter basis is invariant to moisture content. 
# Note2 cont: time cons model is therefore changed to dry tonne basis by multiplying the result with 55/100, i.e. assuming a dry matter content of 55% in the spinelli study. 
# Parameters 

# Other assumptions

CompanyMargin = 10/100; # Assumed profit level

# Time consumption models, t_td productive machine hours per dry tonne
PkW = 350;
t_chip = (0.02 + 13.1 * (PStg * PkW)^-1  + 566 / PkW) * 55/100; # Minutes per dry tonne
t_rep =  (0.584 + 0.00744*(0 / PStg) - 0.385 *1) * 55/100; # minutes pr dry tonne
t_other = (0.2 + 0.0157 * PStg + 4.72*0); # minutes per dry tonne
t_chip_td = (t_chip + t_rep + t_other) / 60; # hours per tonne

# Available machine time:
A_SMh = 1800; #Annual scheduled production time 
MU = 0.8; #Machine utilization, i.e. A.pmh15 / A_SMh
A_PMh15 = A_SMh * MU; # Annual productive machine hours
chipper.A_PMh15 = A_PMh15;

# Annual transport
chipper.A_Prod_td = A_PMh15 / t_chip_td; # Annual 
chipper.A_Prod_m3 = chipper.A_Prod_td /BD; # Annual chipping, m3


# Annual number of cut object
ObjSize = 400; # Assuming 2000 solid m3 per site
chipper.cut_objects = chipper.A_Prod_m3 / ObjSize ;
chipper.A_km = chipper.cut_objects * 40;
## Fuel consumption estimate
# partial estimates, FC = FuelConsumption
## Ref: Spinelli etal 2011, Silva Fennica 45(1) p.89 (table 3) 
# We assume our species have similar characteristics as "softwood" in the study. 
if (Assortment == 1) { # roundwood
  FC_td = 3.2;
} else if (Assortment == 2) { # Logging residues
  FC_td = 3.8;
} else if (Assortment == 3) { #"WholeTrees" # GUESTIMATE!!
  FC_td = 3.5;
}  

FC_td = FC_td * 1.1; # Assuming some extra auxilliary fuel consumption (less favourable conditions, chipper is running idle, etc).  
FC_OPt_day = 5*10/100 ; # Operator transport, 10 km per day, 5 litre / 100 km
FC_OPt_year = FC_OPt_day * A_SMh / 8; # Assuming 8 h average shift length. 

# Fuel cons per year
chipper.FuelCons_machine = FC_td *chipper.A_Prod_td; # Annual fuel consumption of the machine
chipper.FuelConsAUX =  FC_OPt_year ; # Adding FC for auxiliary activities (operator transport, machine relocation)

## Other commodities consumption
chipper.HydrOilCons = 50*  A_PMh15/2000;  # 50 litre hydraulics oil, changed once every 2000 PMh. Ref: Savety.cat.com Maintenance intervals Operation and Maintenance manual. 
chipper.EngineOilCon = 33 * chipper.A_PMh15/2000; # 33 litre engine oil, change every 2000 PMh15. 
chipper.GearOilCons = 10*  chipper.A_PMh15/1000; # 10 litre gear oil, changede every 1000 h


Wheels = 12 *54/200000; # type 285/80R22.5, 54 kg. Life: 10000km, number on trailer: 12 stk.
chipper.TyreCons = (Wheels ) * chipper.A_km; # Annual tyre consumption (kg)

## Cost estimate
# CAPEX part, all estimates made in an spreadsheet machine cost calculator based on assumptions described in the "building blocks forest operations.doc"
Purchase = 2500000; # Purchase cost new machine
chipper.CAPEX_Depr = 188416*A_SMh / 1800; #400' based on linear depreciation and 1800SMh per year. 
chipper.CAPEX_Rate = 68328*A_SMh / 1800;
chipper.CAPEX_Rest = 45000*A_SMh / 1800; # Machine insurance, fees
CAPEX1 = sum(c(chipper.CAPEX_Depr, chipper.CAPEX_Rate, chipper.CAPEX_Rest));
chipper.CAPEX_Margin = CAPEX1 * CompanyMargin;
chipper.CAPEX = sum(c(CAPEX1, chipper.CAPEX_Margin));

# OPEX part
Anleggsdiesel = 10; # kr per liter uten mva. Ref: UnoX listepriser www.unox.no/web/motorist/listepriser.nsf?opendatabase&bedrift
Transpdiesel = 12; # kr per liter uten mva
chipper.OPEX_Fuel = chipper.FuelCons_machine * Anleggsdiesel;# Auxiliary fuel costs are paid through reimbursement to operator, machine truck etc.
chipper.OPEX_Lub = chipper.OPEX_Fuel*0.05; # Tumb rule for cost of machines with limited hydraulics
chipper.OPEX_Rep = Purchase * A_PMh15 / 15000; #Assuming repair cost will equal purchase cost at 15000 PMh15. 
chipper.OPEX_Tires = (22*4500/200000) * chipper.A_km;
chipper.OPEX_OtherGear = 4000 * A_PMh15 / 2000;
chipper.OPEX_Operator = 271*A_SMh; # Includes gross salary, employer tax, OTP, injury insurance and 10#overhead. 
OPEX1 = sum(c(chipper.OPEX_Fuel, chipper.OPEX_Lub, chipper.OPEX_Rep, chipper.OPEX_Tires, chipper.OPEX_OtherGear, chipper.OPEX_Operator));
chipper.OPEX_Margin = OPEX1 * CompanyMargin;
chipper.OPEX = sum(c(OPEX1, chipper.OPEX_Margin));
chipcost_ptd = (chipper.OPEX + chipper.CAPEX ) / chipper.A_Prod_td
chipcost_pm3 = chipcost_ptd * BD
return(chipcost_pm3)

}






rwTruckCost = function( Dist_km, MainSpecies) {
  # Roundwood truck transport function
  # Valid for all kind of roundwood transport on road. 
  # Dist_km= Stratum average transport distance (km)
  # ObjSize = Stratum average cut object size (m3sub)
  # Wood characteristics
  BD = 0.4; #Pine basic density is 400 kgd biomass per m3sob. 
  if (MainSpecies == 2) {  # if spruce
  BD = 0.38;   # 380 kgd biomass per m3sob
  } else if (MainSpecies == 3){ # if birch
  BD = 0.5; # 500 kgd biomass per m3sob
  }
  
  ## Annual productivity estimate
  # Parameters 
  
  speed = 12.7 * log(Dist_km) + 9.3; # average roundtrip speed (km/h), Dist is one-way distance. Ranta etal 2006 The profitability of transporting uncomminuted raw materials in Finland. doi: 10.1016/j.biombioe.2005.11.012
  PLd = 16; # Payload dry wood
  
  # Other assumptions
  Obj_clustering = 3; # Number of cut objects per machine relocations
  CompanyMargin = 10/100; # Assumed profit level
  
  # Time consumption models, trw_m3= productive machine hours per m3 roundwood  
  tload_td = 4.45* 60^-1 ;# terminal time (loading, unloading), hours per td
  t_roundtrip = tload_td*PLd + Dist_km * speed^-1; # Roundtrip time consumption
  trw_td = t_roundtrip / PLd; # Time consumption per dry tonne
  trw_m3sob = trw_td * BD ; # Roundwood PMh15 per m3sob
  
  # Available machine time:
    A_SMh = 1800; #Annual scheduled production time 
  MU = 0.92; #Machine utilization, i.e. A.pmh15 / A_SMh
  A_PMh15 = A_SMh * MU; # Annual productive machine hours
  rw_tr.A_PMh15 = A_PMh15;
  rw_tr.A_PMh = A_PMh15;
  # Annual transport
  rw_tr.A_Prod_RW_m3 = A_PMh15 / trw_m3sob; # Annual roundwood transport, m3
  rw_tr.A_Prod_RW_td = rw_tr.A_Prod_RW_m3*BD; # Annual roundwood transport, td
  
  # Annual time consumption by assortment
  rw_tr.A_PMh_RW = trw_m3sob * rw_tr.A_Prod_RW_m3; # Roundwood
  rw_tr.A_km = Dist_km * 2 * A_PMh15/t_roundtrip; # Annual truck driving distance, i.e. average distance * 2 * annual number of trips. 
  
  # Annual number of cut objects
  rw_tr.cut_objects = rw_tr.A_Prod_RW_m3  ;
  
  ## Fuel consumption estimate
  # partial estimates, FC = FuelConsumption
  FC_RW_td = (0.45+ (0.575*Dist_km * 2 * PLd^-1)); # Roundwood loading and transport, litres pr m3sub. Assuming  
  FC_OPt_day = 5*10/100 ; # Operator transport, 10 km per day, 5 litre / 100 km
  FC_OPt_year = FC_OPt_day * A_SMh / 8; # Assuming 8 h average shift length. 
  
  # Fuel cons per year
  rw_tr.FuelCons_machine = FC_RW_td *rw_tr.A_Prod_RW_td; # Annual fuel consumption of the machine
  rw_tr.FuelConsAUX =  FC_OPt_year ; # Adding FC for auxiliary activities (operator transport, machine relocation)
  
  ## Other commodities consumption
  rw_tr.HydrOilCons = 50*  A_PMh15/2000;  # 50 litre hydraulics oil, changed once every 2000 PMh. Ref: Savety.cat.com Maintenance intervals Operation and Maintenance manual. 
  rw_tr.EngineOilCon = 33 * rw_tr.A_km/90000; # 33 litre engine oil, change every 90000 km. Ref: Wall-chart Volvo Service at a glance 
  rw_tr.GearOilCons = 30*  rw_tr.A_km/400000; # 50 litre gear oil, change every 400000 km. Ref: Wall-chart Volvo Service at a glance 
  
  Steerwheels = 2*72/200000; # type: 385/65R22.5, 72kg, life: 1000000 km, Number on truck: 2 stk
  Drivewheels = 4 * 54/100000; # type 285/80R22.5, 54 kg. Life: 60000km, number on truck: 4 stk. 
  Otherwheels = 16 *54/200000 ;# type 285/80R22.5, 54 kg. Life: 10000km, number on truck: 16 stk.
  rw_tr.TireCons = (Steerwheels + Drivewheels + Otherwheels) * rw_tr.A_km ;# Annual tyre consumption (kg)
  
  ## Cost estimate
  # CAPEX part, all estimates made in an spreadsheet machine cost calculator based on assumptions described in the "building blocks forest operations.doc"
  Purchase = 2500000; # Purchase cost new machine
  rw_tr.CAPEX_Depr = 188416*A_SMh / 1800; #400' based on linear depreciation and 1800SMh per year. 
  rw_tr.CAPEX_Rate = 68328*A_SMh / 1800;
  rw_tr.CAPEX_Rest = 45000*A_SMh / 1800; # Machine insurance, fees
  CAPEX1 = sum(rw_tr.CAPEX_Depr, rw_tr.CAPEX_Rate, rw_tr.CAPEX_Rest);
  rw_tr.CAPEX_Margin = CAPEX1 * CompanyMargin;
  rw_tr.CAPEX = sum(CAPEX1, rw_tr.CAPEX_Margin);
  
  # OPEX part
  Anleggsdiesel = 10; # kr per liter uten mva. Ref: UnoX listepriser www.unox.no/web/motorist/listepriser.nsf?opendatabase&bedrift
  Transpdiesel = 12; # kr per liter uten mva
  rw_tr.OPEX_Fuel = rw_tr.FuelCons_machine * Transpdiesel;# Auxiliary fuel costs are paid through reimbursement to operator, machine truck etc.
  rw_tr.OPEX_Lub = rw_tr.OPEX_Fuel*0.05; # Tumb rule for cost of lubricates for trucks having boom loader
  rw_tr.OPEX_Rep = Purchase * A_PMh15 / 15000; #Assuming repair cost will equal purchase cost at 15000 PMh15. 
  rw_tr.OPEX_Tires = (22*4500/200000) * rw_tr.A_km;
  rw_tr.OPEX_OtherGear = 4000 * A_PMh15 / 2000;
  rw_tr.OPEX_Operator = 271*A_SMh; # Includes gross salary, employer tax, OTP, injury insurance and 10#overhead. 
  OPEX1 = sum(rw_tr.OPEX_Fuel, rw_tr.OPEX_Lub, rw_tr.OPEX_Rep, rw_tr.OPEX_Tires, rw_tr.OPEX_OtherGear, rw_tr.OPEX_Operator);
  rw_tr.OPEX_Margin = OPEX1 * CompanyMargin;
  rw_tr.OPEX = sum(OPEX1, rw_tr.OPEX_Margin);
  
  
  
  Totex = sum(c(rw_tr.CAPEX,rw_tr.OPEX));
  CTotptd = Totex/rw_tr.A_Prod_RW_td;
  CTotpm3 = Totex/rw_tr.A_Prod_RW_m3 ;
  
  return(CTotpm3)
  
}
