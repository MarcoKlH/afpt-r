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
%% ~put references to the literature/web site here ~
}
\author{Marco Klein Heerenbrink}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{computeChemicalPower}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
