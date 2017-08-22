\name{computeChemicalPower}
\alias{computeChemicalPower}
\alias{computeChemicalPower.power.mechanical}
\alias{computeChemicalPower.numeric}
\title{Convert mechanical power to chemical power}
\description{
Computes the chemical power, i.e. the rate at which chemical energy is consumed, during flight. It takes into account the basal metabolic rate, and the energy needed by the flight muscles to provide the mechanical power required for flight.
}
\usage{
\method{computeChemicalPower}{power.mechanical}(power.mech, bird, ...)
\method{computeChemicalPower}{numeric}(power.mech, bird, ...)
}
\arguments{
  \item{power.mech}{mechanical power (either numeric (W) or as an mechanical power object (class power.mechanical)}
  \item{bird}{ object describing the relevant morphological parameters of the bird (or bat); this object should be created using the \code{\link{Bird}} constructor.}
  \item{\dots}{optional arguments (none yet)}
}
\details{
Chemical power is computed as
\deqn{P_\mathrm{chem} = R(\frac{P_\mathrm{mech}}{\eta} + \mathrm{BMR})}{ P.chem = R * (P.mech/eta + BMR)} as described by \cite{Pennycuick (2008)}. Here \eqn{R} is the respiration factor, \eqn{\eta}{eta} is the muscle conversion efficiency and \eqn{\mathrm{BMR}}{BMR} the basal metabolic rate, see \code{\link{Bird}}.
}
\value{
Chemical power of same type as inpute \code{power.chem}.
}
\references{
Pennycuick, C. J. (2008). \emph{Modelling the flying bird.} Amsterdam, The Netherlands: Elsevier.}
\author{
Marco Klein Heerenbrink
}
\seealso{
\code{\link{Bird}}, \code{\link{computeFlappingPower}}, \code{\link{mech2chem}}, \code{\link{chem2mech}}
}
\examples{
## Define a bird:
myBird = Bird(
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

## for maximum continuous power
power.max <- computeAvailablePower(myBird)
print(power.max)
#   [1] 5.233528

## convert to chemical power
power.max.chem <- computeChemicalPower(power.max,myBird)
print(power.max.chem)
#   [1] 27.28913

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
