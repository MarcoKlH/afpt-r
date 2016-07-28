\name{findMaximumPowerSpeed}
\alias{findMaximumPowerSpeed}
\title{Finds speed for which power required equals maximum available power}
\description{Numerically find the airspeed for which required power equals \code{maximumPower}.}
\usage{
findMaximumPowerSpeed(bird, maximumPower, lower, upper, ...)
}
\arguments{
  \item{bird}{bird description object (see \code{\link{Bird}})}
  \item{maximumPower}{numeric value for maximum available mechanical power}
  \item{lower}{lower bound for search range airspeed (m/s)}
  \item{upper}{upper bound for search range airspeed (m/s)}
  \item{\dots}{optional arguments to \code{\link{computeFlappingPower}}}
}
\details{
Prepares arguments for a call to \code{\link{uniroot}}. The function searches for an airspeed between \code{lower} and \code{upper} that matches the specified maximum power available.
}
\value{
Data frame of class \code{power.mechanical}
\item{speed}{airspeed for which power matches maximum power}
\item{power}{aerodynamic (mechanical) power matching maximum power}
\item{strokeplane}{ optimized or prescribed strokeplane angle in degrees (from vertical)}
\item{amplitude}{ optimized peak amplitude in degrees (see \code{\link{amplitude}})}
\item{\dots}{see \code{\link{computeFlappingPower}} for other variables}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Marco Klein Heerenbrink
}
\note{
Typically this function would be used to find the maximum speed, but may in some cases also be used for the minimum flight speed. However, note that the low speed limit is likely limited by other constraints as well (e.g. stall speed).
}

\seealso{
\code{\link{uniroot}}
}
\examples{
## Define a bird:
myBird <- Bird(
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

Paero.available  <- computeAvailablePower(myBird)

## find maximum speed:
Vmin <- 5
Vmax <- 30
Paero.maxSpeed <- findMaximumPowerSpeed(myBird,Paero.available,Vmin,Vmax)
print(Paero.maxSpeed[c('speed','power','amplitude','strokeplane','frequency')])
#        speed    power amplitude strokeplane frequency
#   1 16.55791 5.233528  50.20133    6.655102  5.948083
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
