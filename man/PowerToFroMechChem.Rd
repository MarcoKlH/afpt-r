\encoding{utf8}
\name{PowerToFroMechChem}
\alias{mech2chem}
\alias{chem2mech}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert between mechanical and chemical power
}
\description{
Functions convert between mechanical and chemical power
}
\usage{
mech2chem(power.mech,bird,...)
chem2mech(power.chem,bird,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{power.mech}{Numerical value for mechanical power}
  \item{power.chem}{Numerical value for chemical power}
  \item{bird}{ object describing the relevant morphological parameters of the bird (or bat); this object should be created using the \code{\link{Bird}} constructor.}
  \item{\dots}{ optional arguments (none yet)}
}
\details{
Chemical power is computed as
\deqn{P_\mathrm{chem} = R(\frac{P_\mathrm{mech}}{\eta} + \mathrm{BMR})}{ P.chem = R * (P.mech/eta + BMR)} as described in \cite{Pennycuick 2008}. Here \eqn{R} is the respiration factor, \eqn{\eta}{eta} is the muscle conversion efficiency and \eqn{\mathrm{BMR}}{BMR} the basal metabolic rate, see \code{\link{Bird}}.

Mechanical power is simply calculated inversely:
\deqn{P_\mathrm{mech} = \eta(\frac{P_\mathrm{chem}}{R} - \mathrm{BMR})}{ P.chem = eta * (P.chem/R - BMR)}
}
\value{
Numerical value of either chemical power (\code{mech2chem()}) or mechanical power (\code{chem2mech()}).
}
\references{
Pennycuick, C. J. (2008). \emph{Modelling the flying bird.} Amsterdam, The Netherlands: Elsevier.
}
\author{Marco Klein Heerenbrink}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{computeChemicalPower}}
}
\examples{
## Define a bird:
myBird = Bird(
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

## define a speed range
speedrange <- seq(5,14,length.out=5)

## compute aerodynamic power for that speed range:
Paero <- computeFlappingPower(myBird,speedrange)
Pchem <- Paero
Pchem$power <- mech2chem(Paero$power,myBird)
print(Pchem[c("speed","power","frequency","strokeplane")])
#   speed    power frequency strokeplane
# 1  5.00 15.60151  5.948083    46.56887
# 2  7.25 12.44362  5.948083    31.89129
# 3  9.50 12.79900  5.948083    22.51896
# 4 11.75 15.36721  5.948083    16.49120
# 5 14.00 19.82915  5.948083    12.09174

Pmech <- Pchem
Pmech$power <- chem2mech(Pchem$power,myBird)
print(Pmech[c("speed","power","frequency","strokeplane")])
#   speed    power frequency strokeplane
# 1  5.00 2.789751  5.948083    46.56887
# 2  7.25 2.129466  5.948083    31.89129
# 3  9.50 2.203773  5.948083    22.51896
# 4 11.75 2.740763  5.948083    16.49120
# 5 14.00 3.673714  5.948083    12.09174
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
