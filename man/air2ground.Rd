\name{air2ground}
\alias{air2ground}
\title{
Compute groundspeed}
\description{
Computes groundspeed from airspeed and wind.}
\usage{
air2ground(airSpeed, windSpeed = 0, windDir = 0, climbAngle = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{airSpeed}{
airspeed}
  \item{windSpeed}{
windspeed}
  \item{windDir}{
wind direction relative to (intended) track direction in degrees}
  \item{climbAngle}{
climb angle in degrees}
}
\value{
\item{driftAngle}{Angle between airspeed and groundspeed}
\item{groundSpeed}{Speed over ground}
}
\author{
Marco Klein Heerenbrink
}
