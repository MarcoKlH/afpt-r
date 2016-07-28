\encoding{utf8}
\name{reducedFrequency}
\alias{reducedFrequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to compute reduced frequency}
\description{
This function computes the reduced frequency based on \code{wingSpan} (\eqn{b}{b}), wingbeat \code{frequency} (\eqn{f}{f}) and \code{speed} (\eqn{U}{U}):
\eqn{k_f = \frac{2\pi b f}{U}}{kf = 2 * pi * b * f / U}.
}
\usage{
reducedFrequency(wingSpan, frequency, speed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wingSpan}{Tip-to-tip distance of the fully spread wing (m)}
  \item{frequency}{Wingbeat frequency (1/s)}
  \item{speed}{Airspeed (m/s)}
}
\details{
This parameter is the ratio of the wingspan to the wavelength of the convected wake. For very high reduced frequencies, the wake of one wingbeat is relatively short compared to the wingspan, meaning that previous wingbeats have a large influence on the aerodynamics of the current wingbeat. When the reduced frequency is low, there is relatively little interaction between the wingbeats.

This wingspan based reduced frequency should not be confused with the chord based (or half chord) based reduced frequency. That definition serves a similar function, however, it relates to the effect of unsteadyness on the aerofoil (i.e. it is somewhat like the 2D equivalent).

Another related parameter of unsteadyness, often mentioned in relation to animal flight, is the Strouhal number, representing the ratio of the amplitude of the wingbeat to the wavelength of the wake. This term is historically related to vortex shedding.
}
\value{
Numeric value
}
\references{
Klein Heerenbrink, M., Johansson, L. C. and Hedenström, A. 2015 Power of the wingbeat: modelling the effects of flapping wings in vertebrate flight. \emph{Proc. R. Soc. A} \bold{471}, 2177 \doi{10.1098/rspa.2014.0952}
}
\author{
Marco Klein Heerenbrink
}

\seealso{
\code{\link{computeFlappingPower}}
}
\examples{
kf <- reducedFrequency(
  wingSpan = 0.67,
  frequency = 4,
  speed = 9
)
kf
# [1] 1.870993
}
