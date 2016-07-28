\name{findMaximumClimbRate}
\alias{findMaximumClimbRate}
\title{Find maximum climb rate}
\description{Numerically find the maximum attainable climb rate.}
\usage{
findMaximumClimbRate(bird, maximumPower, speed, ...)
}
\arguments{
  \item{bird}{bird description object (see \code{\link{Bird}})}
  \item{maximumPower}{numeric value for maximum available mechanical power}
  \item{speed}{airspeed for which to compute the maximum climbrate}
  \item{\dots}{optional arguments for \code{\link{computeFlappingPower}}}
}
\details{
The function searches for a climb angle between -90 and 90 degrees that matches the specified maximum power available. If no \code{speed} provided, the function will also find the optimal airspeed for maximum climbrate.
}
\value{ Data frame of class \code{power.mechanical}
\item{speed}{airspeed either prescribed or optimized for maximum climbrate}
\item{power}{aerodynamic (mechanical) power matching maximum power}
\item{\dots}{see \code{\link{computeFlappingPower}} for other variables}
\item{climbAngle}{ angle between flightpath and horizontal plane in degrees  }
\item{climbRate}{ rate of vertical climb }
}
\author{Marco Klein Heerenbrink}
\note{The function uses climb angle, rather than climb rate, in the search algorithm, to ensure that climb rate is always less than the airspeed (i.e. in a vertical climb the climb rate will simply equal airspeed). The actual climb rate is maximized by maximizing the product of climb angle and airspeed. However, in practice, the airspeed for best climb rate will be close to the minimum power airspeed, where the power margin is largest.}
\seealso{
\code{\link{uniroot}}
}
\examples{
## Define a bird:
myBird = Bird(
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

## maximum power available:
Paero.available <- computeAvailablePower(myBird)

climbSpeed <- 8 #  airspeed during climb

## find maximum climbrate:
Paero.climb <- findMaximumClimbRate(myBird,Paero.available,climbSpeed)
print(Paero.climb[c('speed','amplitude','frequency','climbRate')])
#     speed amplitude frequency climbRate
#   1     8   54.8201  5.948083  1.159171

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
