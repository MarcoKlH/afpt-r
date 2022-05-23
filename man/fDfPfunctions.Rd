\encoding{utf8}
\name{fDfPfunctions}
\alias{fD.ind}
\alias{fD.pro0}
\alias{fD.pro2}
\alias{fP.ind}
\alias{fP.pro0}
\alias{fP.pro2}
\title{ Coefficient for thrust dependency of drag and power factors }
\description{
Computes the thrust requirement dependency factor for drag and power factors in flapping flight based on reduced frequency (\code{kf}) and strokeplane angle (\code{phi}).
}
\usage{
fD.ind(kf, phi)
fD.pro0(kf, phi)
fD.pro2(kf, phi)
fP.ind(kf, phi)
fP.pro0(kf, phi)
fP.pro2(kf, phi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  {Using \eqn{f} for wingbeat frequency, \eqn{b} for wingspan, and \eqn{U} for air speed:}
  \item{kf}{reduced frequency (\eqn{k_f = \frac{2 \pi f b}{U}}{kf = 2 * \pi * f *b / U}); valid range between 1 and 6}
  \item{phi}{strokeplane angle in radians; valid range between 0 and 0.87 rad (50 deg)}
}
\details{
Flapping of the wings alters the drag components on the wing. A drag component in flapping flight can be related to the drag component in non-flapping flight as \eqn{D = k_D D^\prime}{D = kD * Dnf }. The factor \eqn{k_D}{kD} depends on reduced frequency \eqn{k_f}{kf}, strokeplane angle \eqn{\phi}{phi} and the thrust-to-lift ratio \eqn{T/L}: \eqn{k_D = 1 + f_D(k_f,\phi) \frac{T}{L}}{ kD = 1 + fD(kf,phi) * T/L }. Functions \code{fD.ind},\code{fD.pro0} and \code{fD.pro2} compute \eqn{f_D(k_f,\phi)}{fD(kf,phi)} for induced drag, zero lift profile drag and lift dependent profile drag, respectively.

Similarly, the flapping power components can be computed as: \eqn{P = k_P D^\prime U}{P = k_P * Dnf * U}, again with \eqn{k_P = 1 + f_P(k_f,\phi) \frac{T}{L}}{ kP = 1 + fP(kf,phi) * T/L }. Functions \code{fP.ind},\code{fP.pro0} and \code{fP.pro2} compute \eqn{f_P(k_f,\phi)}{fP(kf,phi)} for induced power, zero lift profile power and lift dependent profile power, respectively.
}
\value{
Numeric value
}
\references{
Klein Heerenbrink, M., Johansson, L. C. and Hedenström, A. 2015 Power of the wingbeat: modelling the effects of flapping wings in vertebrate flight. \emph{Proc. R. Soc. A} \bold{471}, 2177 \doi{10.1098/rspa.2014.0952}
}
\author{Marco Klein Heerenbrink}
\note{
Thrust requirement is the sum of all drag components in flapping flight divided by the lift. This means the thrust requirement itself is a function of the values of \eqn{f_D}{fD}.
}
\seealso{
\code{\link{computeFlappingPower}}
}
\examples{
## reduced frequency
kf <- 2*pi*4/10 # 4 Hz at 10 m/s

## strokeplane angle
phi <- 20*pi/180 # 20 degrees

## thrust ratio
TL <- 0.2

## induced drag factor:
fDind <- fD.ind(kf,phi)
kDind <- 1 + fDind*TL
print(kDind)
#   [1] 1.623659

## zero lift drag factor:
fDpro0 <- fD.pro0(kf,phi)
kDpro0 <- 1 + fDpro0*TL
print(kDpro0)
#   [1] 1.014899

## lift dependent profile drag factor:
fDpro2 <- fD.pro2(kf,phi)
kDpro2 <- 1 + fDpro2*TL
print(kDpro2)
#   [1] 1.511107

## induced power factor:
fPind <- fP.ind(kf,phi)
kPind <- 1 + fPind*TL
print(kPind)
#   [1] 1.996891

## zero lift power factor:
fPpro0 <- fP.pro0(kf,phi)
kPpro0 <- 1 + fPpro0*TL
print(kPpro0)
#   [1] 1.076046

## lift dependent profile power factor:
fPpro2 <- fP.pro2(kf,phi)
kPpro2 <- 1 + fPpro2*TL
print(kPpro2)
#   [1] 1.811983


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{  }
