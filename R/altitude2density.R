altitude2density <- function(altitude=0) {
  fc <- FLIGHTCONDITION
  rho0 <- fc$density
  g0 <- fc$gravity
  R <- fc$gasconstant
  T0 <- fc$temperature
  a <- fc$tempgrad


  rho0*(1 + a*altitude/T0)^-(g0/R/a + 1)
}
