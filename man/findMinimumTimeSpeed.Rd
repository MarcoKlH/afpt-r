\encoding{utf8}
\name{findMinimumTimeSpeed}
\alias{findMinimumTimeSpeed}
\title{Find speed for migration time minimization}
\description{
This function performs a numerical optimization to find the airspeed for which \eqn{\frac{P+P_\mathrm{dep}}{U}}{(P+Pdep)/U} is minimum..
}
\usage{
findMinimumTimeSpeed(bird,
  EnergyDepositionRate=1.5*bird$basalMetabolicRate,
  lower=NULL,upper=NULL,
  windSpeed=0,windDir=0,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bird}{bird description object (see \code{\link{Bird}})}
  \item{EnergyDepositionRate}{The rate at which the bird accumulates energy at stopover sites}
  \item{lower}{ lower speed limit (optional)}
  \item{upper}{ upper speed limit (optional)}
  \item{windSpeed}{ wind magnitude (in m/s; optional)}
  \item{windDir}{ wind direction (in degrees; optional)}
  \item{\dots}{ optional arguments: \code{climbAngle} (in degrees), and optional arguments for \code{\link{computeFlappingPower}}. }
}
\details{
This function performs a numerical optimization to find the airspeed that minimizes the combination of flight time and time required to (re)gain the energy reserves to cover the flight cost. If the bird would fly faster, it would need to spend more time refueling. If it flew slower, the reduced refueling time that comes with the lower cost of transport does not offset the longer flight time. Mathematically this problem works out as  minimizing \eqn{\frac{P+P_\mathrm{dep}}{U}}{(P+Pdep)/U} \cite{Hedenström 1998}, which is technically the same optimization as for the maximum range speed (see details \code{\link{findMaximumRangeSpeed}}). The default energy deposition rate, the rate at which a bird accumulates energy during a stopover, is set to 1.5 times the basal metabolic rate (\cite{Lindström 1991}).
}
\value{Returns data.frame (power.chemical) of flight performance at maximum range speed for \code{bird}.}
\references{
Lindström, Å. (1991) Maximum fat deposition rates in migrating birds. \emph{Ornis Scand.} \bold{22}, 12-19 (doi:10.2307/3676616)

Hedenström, A. & Alerstam, T. (1997) Optimum fuel loads in migratory birds: distinguishing between time and energy minimization. \emph{J. Theor. Biol.} \bold{189}, 227–34. (doi:10.1006/jtbi.1997.0505)

Hedenström, A. & Alerstam, T. (1998) How fast can birds migrate? \emph{J. Avian Biol.} \bold{29}, 424-432. (doi:10.2307/3677161)
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

minimumTimeSpeed <- findMinimumTimeSpeed(myBird,1.5*myBird$basalMetabolicRate)
minimumTimeSpeed[c('speed','speed.migration',
    'power','power.chem','power.dep',
    'strokeplane','amplitude','frequency')]
#     speed speed.migration   power power.chem power.dep strokeplane amplitude frequency
#  11.74944        1.962213 2.74058   15.36634  3.080752    16.49244  38.03366  5.948083


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ migration }
\keyword{ energy optimization }% __ONLY ONE__ keyword per line
