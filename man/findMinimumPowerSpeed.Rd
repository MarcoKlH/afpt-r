\name{findMinimumPowerSpeed}
\alias{findMinimumPowerSpeed}
\title{Find speed for minimum power}
\description{.}
\usage{
findMinimumPowerSpeed(bird, lower, upper, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bird}{bird description object (see \code{\link{Bird}})}
  \item{lower}{ lower speed limit (optional)}
  \item{upper}{ upper speed limit (optional)}
  \item{\dots}{ optional arguments for \code{computeFlappingPower()}}
}
\details{
This is pretty much just a call to \code{optimize}.}
\value{powercurve object (\code{funCalcPower} evaluated for the minimum speed)}
\author{Marco Klein Heerenbink}
\seealso{\code{\link{optimize}}}
\examples{
## Define a bird:
myBird = Bird(
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

minimumPowerSpeed.aero <- findMinimumPowerSpeed(myBird)
minimumPowerSpeed.aero[c('speed','power','strokeplane','amplitude','frequency')]
#        speed    power
#   1 8.023133 2.098655
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
