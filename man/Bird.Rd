\encoding{utf8}
\name{Bird}
\alias{Bird}
\alias{Bird.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bird description}
\description{
This function creates a bird description object, which is basically just a list with predefined variable names. It is named a bird object, but could also contain a description of a bat or insect. Minimal input required to construct a bird are body mass, wing span and wing area (or wing aspect ratio). Other required variables will then be given default values, or they will be estimated from allometric relations from literature.
}
\usage{
Bird(massTotal, wingSpan, wingArea, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{massTotal}{ Total mass that needs to be lifted in flight in kg }
  \item{wingSpan}{ The maximum distance between the wingtips in meters }
  \item{wingArea}{ The area of the fully stretched wings including the root area (left wing, right wing and area in between the wing roots) }
  \item{\dots}{ Any other properties of a valid bird object (see details) }
}
\details{
This function sets up a list of properties of a bird. This definition of the bird is then used by the other functions in the package to estimate flight performance. At least three properties need to be specified: \code{massTotal}, \code{wingSpan} and \code{wingArea}. Either \code{wingSpan} or \code{wingArea} could be replaced by \code{aspectRatio}; the missing variable will then be computed. If no other properties are specified, default values will be used. Wingspan and wingarea should be measured from the maximally stretched out wing as described in \cite{Pennycuick (2008)}: wingspan as the maximum distance between the wingtips and wingarea as the area from a trace including the root area (where the body is).

To specify custom properties, these can simply be added as additional arguments to the function. Note that \code{massTotal} needs to be the sum of \code{massLoad}, \code{massFat} and \code{massEmpty}. The function will recompute the total mass if the specified masses are inconsistent. Allometric relations use the empty weight. Muscle mass is part of the empty mass, and as such it is represented by \code{muscleMass} as a fraction. It is used in the estimation of the mechanical power available for flight (together with the muscle properties \code{coef.activeStrain} and \code{coef.isometricStress}). The variable \code{type} is used for selected  allometric relationships that are specific to that particular group. Currently, \code{bodyFrontalArea} distinguishes between \code{'passerine'} and anything else and \code{basalMetabolicRate} distinguishes between \code{'passerine'}, \code{'seabird'}, \code{'bat'} and anything else.

\tabular{lll}{
\code{name} \tab String \tab Common name\cr
\code{name.scientific} \tab Sring \tab Scientific name\cr
\code{source} \tab String \tab Source for information\cr
\code{massLoad} \tab Numeric \tab Additional mass the bird is carrying (kg); \bold{0}\cr
\code{massFat} \tab Numeric \tab Fat mass, i.e. fuel (kg); \bold{0}\cr
\code{massEmpty} \tab Numeric \tab Empty mass, i.e. total mass - fat mass - load mass (kg)\cr
\code{muscleFraction} \tab Numeric \tab Fraction [0,1] of empty mass that makes up flight muscle; \bold{0.17}*\cr
\code{type} \tab String \tab Type of bird \bold{\code{'other'}}*, \code{'passerine'}*, \code{'seabird'}, \code{'bat'}\cr
\code{bodyFrontalArea} \tab Numeric \tab Reference body frontal area used for body drag (m2)\cr
\code{wingbeatFrequency} \tab Numeric \tab Typical wingbeat frequency (Hz)\cr
\code{coef.profileDragLiftFactor} \tab Numeric \tab Coefficient for lift dependent profile drag; \bold{0.03} (\cite{Klein Heerenbrinkn et al. 2015})\cr
\code{coef.bodyDragCoefficient} \tab Numeric \tab Drag coefficient related to body frontal area; \bold{0.2}**\cr
\code{coef.conversionEfficiency} \tab Numeric \tab Efficiency Chemical to Mechanical energy; \bold{0.23}*\cr
\code{coef.respirationFactor} \tab Numeric \tab Multiplyer for metabolic overhead respiration; \bold{1.1}*\cr
\code{coef.activeStrain} \tab Numeric \tab Muscle duty cycle factor; \bold{0.26}*\cr
\code{coef.isometricStress} \tab Numeric \tab Maximum force produced per cross section muscle (Pa); \bold{400000} (upper limit from \cite{Pennycuick & Rezende 1984})\cr
\code{basalMetabolicRate} \tab Numeric \tab Minimum energy consumption required for sustain life functions (W) *.
}
* as in Flight 1.25 (\cite{Pennycuick 2008})

** Large body of data supporting higher body drag coefficients (>0.2) than in Flight 1.25 (0.1), e.g. \cite{Pennycuick et al. (1988)}, \cite{Hedenström & Liechti (2001)}, \cite{Henningsson & Hedenström (2011)} and \cite{KleinHeerenbrink et al. (2016)}
}
\value{
bird object with variables required by the various power estimating functions (e.g. \code{\link{computeFlappingPower}}).
}
\references{
Hedenström, A. & Liechti, F. (2001) Field estimates of body drag coefficient on the basis of dives in passerine birds. \emph{J. Exp. Biol.} \bold{204}, 1167–75.

Henningsson, P. & Hedenström, A. (2011) Aerodynamics of gliding flight in common swifts. \emph{J. Exp. Biol.} \bold{214}, 382–93. \doi{10.1242/jeb.050609}

Klein Heerenbrink, M., Johansson, L. C. & Hedenström, A. (2015) Power of the wingbeat: modelling the effects of flapping wings in vertebrate flight. \emph{Proc. R. Soc. A} \bold{471}. \doi{10.1098/rspa.2014.0952}

KleinHeerenbrink, M., Warfvinge, K. & Hedenström, A. (2016) Wake analysis of aerodynamic components for the glide envelope of a jackdaw (\emph{Corvus monedula}). \emph{J. Exp. Biol.} \bold{219}, 1572–1581. \doi{10.1242/jeb.132480}

Pennycuick, C. J. & Rezende, M. A. (1984) The specific power output of aerobic muscle, related to the power density of mitochondria. \emph{J. Exp. Biol.}, \bold{108}, {377–392}.

Pennycuick, C. J., Obrecht III, H. H. & Fuller, M. R. (1988) Empirical estimates of body drag of large waterfowl and raptors. \emph{J. Exp. Biol.} \bold{135}, 253–264.

Pennycuick, C. J. (2008). \emph{Modelling the flying bird.} Amsterdam, The Netherlands: Elsevier.
}
\author{Marco Klein Heerenbrink}
\seealso{
\code{\link{computeAvailablePower}}, \code{\link{computeChemicalPower}}, \code{\link{computeFlappingPower}}, \code{\link{computeBodyFrontalArea}}, etc.
}
\examples{
myBird = Bird(
  massTotal = 0.215,
  wingSpan = 0.67,
  wingArea = 0.0652,
  name = 'jackdaw',
  type =  'passerine'
)
print(myBird)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
