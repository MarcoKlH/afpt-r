\encoding{utf8}
\name{amplitude}
\alias{amplitude}
\title{Flapping flight optimal amplitude}
\description{This function returns the angular peak amplitude of the flapping motion, optimized for minimum induced power for prescribed reduced frequency (kf), strokeplane angle (phi), and thrust-to-lift ratio (TL).}
\usage{amplitude(kf, phi, TL)}
\arguments{
  Using \eqn{f} for wingbeat frequency, \eqn{b} for wingspan, and \eqn{U} for air speed:
  \item{kf}{reduced frequency (\eqn{k_f = \frac{2 \pi f b}{U}}{kf = 2 * \pi * f *b / U}); valid range between 1 and 6}
  \item{phi}{strokeplane angle in radians; valid range between 0 and 0.87 rad (50 deg)}
  \item{TL}{thrust requirement or the trust-to-lift ratio; valid range between 0 and 0.3}
}
\value{
  Angular peak amplitude of the flapping motion in degrees.
}
\references{
Klein Heerenbrink, M., Johansson, L. C. and Hedenström, A. 2015 Power of the wingbeat: modelling the effects of flapping wings in vertebrate flight. \emph{Proc. R. Soc. A} \bold{471}, 2177 \doi{10.1098/rspa.2014.0952}
}
\author{Marco Klein Heerenbrink}
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
## wingbeat amplitude
theta <- amplitude(kf,phi,TL)
print(theta)
#   [1] 49.17679
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
