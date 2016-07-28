\name{altitude2density}
\alias{altitude2density}
\title{Compute density in International Standard Atmopshere}
\description{
This function computes the air density at a specified altitude in the Troposphere of the International Standard Atmosphere.}
\usage{
altitude2density(altitude = 0)
}
\arguments{
  \item{altitude}{ (geopotential) altitude in meters above sealevel. }
}
\details{
  \eqn{\rho = \rho_0 (1 + a \frac{h}{T_0}) ^ {-\frac{g_0}{Ra}+1}}{rho = rho0*(1 + a*altitude/T0)^-(g0/R/a + 1)}
  with \eqn{\rho_0}{rho0} = 1.225 kg/m3, \eqn{a}{a} = -0.0065 K/m, \eqn{h}{altitude} geopotential altitude in meters, \eqn{g_0}{g0} = 9.80665 m/s2, and \eqn{R}{R} = 287.1 J/Kg/K.
}
\value{
Numerical value or array for the density in kg/m3
}
\references{
\href{http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf}{U.S. Standard Atmosphere, 1976, U.S. Government Printing Office, Washington, D.C.}
}
\author{
M. Klein Heerenbrink
}


\examples{
altitude <- seq(0,3000,100) # meters above sealevel
density <- altitude2density(altitude)

}
