\encoding{utf8}
\name{findMaximumRangeSpeed}
\alias{findMaximumRangeSpeed}
\title{Find maximum range speed}
\description{
This function performs a numerical optimization to find the airspeed for which \eqn{\frac{P}{U}}{P/U} is minimum. For this it uses the function \code{optimize}.
}
\usage{
findMaximumRangeSpeed(bird, lower, upper, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bird}{bird description object (see \code{\link{Bird}})}
  \item{lower}{ lower speed limit (optional)}
  \item{upper}{ upper speed limit (optional)}
  \item{windSpeed}{ wind magnitude (in m/s; optional)}
  \item{windDir}{ wind direction (in degrees; optional)}
  \item{\dots}{ optional arguments: \code{climbAngle} (in degrees), and optional arguments for \code{\link{computeFlappingPower}}. }
}
\details{
This function performs a numerical optimization to find the airspeed for which \eqn{\frac{P}{U}}{P/U} is minimum. For this it uses the function \code{optimize}. This airspeed is searched for between \code{lower}  and  \code{upper} (if not provided, it will make a guess based on \code{bird}). Flying in wind changes the ground speed, and therefore the optimum flight speed for maximum range. This can be taken into account through the optional arguments for wind magnitude (\code{windSpeed} in m/s) and wind direction relative to the track direction (\code{windDir} in degrees; \code{windDir = 0} tail wind); see e.g. \cite{Liechti et al. 1994}.
}
\value{Returns data.frame (power.chemical) of flight performance at maximum range speed for \code{bird}.}
\references{
Liechti, F., Hedenström, A. and Alerstam, T. (1994). Effects of Sidewinds on Optimal Flight Speed of Birds. \emph{J. Theor. Biol.} \bold{170}, 219–225.
}
\author{
Marco Klein Heerenbrink
}
\seealso{
\code{\link{computeChemicalPower}}, \code{\link{computeFlappingPower}}
}
\examples{
## Define a bird:
myBird = Bird(
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

maximumRangeSpeed.chem <- findMaximumRangeSpeed(myBird)
maximumRangeSpeed.chem[c('speed','power','strokeplane','amplitude','frequency')]
#        speed    power strokeplane amplitude frequency
#   1 11.01736 14.36211    18.15247  36.73515  5.948083

maximumRangeSpeed.chem.wind <- findMaximumRangeSpeed(
  myBird,
  windSpeed = 5,
  windDir = 90
)
maximumRangeSpeed.chem.wind[c('speed','power','strokeplane','amplitude','frequency')]
#        speed    power strokeplane amplitude frequency
#   1 11.81036 15.51402     16.2951  38.21434  5.948083
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
