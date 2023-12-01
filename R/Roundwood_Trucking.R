



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
