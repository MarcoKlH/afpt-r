FLIGHTCONDITION <- list(
  pressure = 101325, # ISA0 pressure
  gasconstant = 287.053, # ISA0 gas constant
  temperature = 288.15, # ISA0 air temperature
  tempgrad = -0.0065, # ISA0 troposphere temperature gradient
  density = 1.225, # ISA0 air density
  gravity = 9.80665, # ISA0 gravity
  viscosity = 14.6E-6, # ISA0 Kinematic viscosity
  windSpeed = 0,
  windDir = 0
)
# ISA0 values according to U.S.Standard Atmosphere 1976 (NASA-TM-X-74335)


FLAPPINGMODELCOEFFS <- list(
  D.ind =  list( p00 = 9.250, p02 = 2.931, p10 = 0,     p11 =-1.969, p12 = 0,     p20 = 0     ),
  D.pro0 = list( p00 =-0.590, p02 =-1.145, p10 = 0.239, p11 = 0.197, p12 = 0.446, p20 =-0.715 ),
  D.pro2 = list( p00 = 9.298, p02 = 1.301, p10 =-0.659, p11 =-1.521, p12 = 0,     p20 = 0     ),
  P.ind =  list( p00 = 7.517, p02 = 4.913, p10 = 2.573, p11 =-1.259, p12 = 0,     p20 = 0,    r = 1.135),
  P.pro0 = list( p00 =-1.101, p02 =-1.183, p10 = 0.512, p11 = 0.227, p12 = 0.431, p20 = 0,    r = 0    ),
  P.pro2 = list( p00 = 8.851, p02 = 2.807, p10 = 1.354, p11 =-0.943, p12 = 0,     p20 = 0,    r = 1.201),
  A = list(
    p00 = 1.277, p01 = 0,     p02 = 0,
    p10 = 6.214, p11 = 0,     p12 = 3.631,
    p40 = 227.8, p41 = 0,     p42 = 1481,
    q00 = 0.0107,q01 = 0.0565,q02 =-0.0169, r = 2.620
  )
)

devtools::use_data(FLIGHTCONDITION,FLAPPINGMODELCOEFFS,internal=TRUE,overwrite=TRUE)



