\encoding{utf8}
\name{computeAvailablePower}
\alias{computeAvailablePower}
\title{ Compute available power }
\description{ Estimation of maximum available power available from the muscles.  }
\usage{computeAvailablePower(bird, maxPowerAero, ...)}
\arguments{
  \item{bird}{bird description object (see \code{\link{Bird}})}
  \item{maxPowerAero}{maximum continuous power}
  \item{\dots}{optional arguments (none yet)}
}
\details{
Available power is determined as a muscle property. It is assumed that part of the muscles tissue is chemically active (mitochondria), providing the required ATP energy to the mechanically active tissue (myofibrils). The fraction of mitochondria determines the maximum sustainable power output from the muscles. With a higher fraction of myofibrils, the muscles can produce more power, but only in a short burst, until all ATP runs out.

If only a \code{\link{Bird}} object is provided, the function will assume that maximum power equals maximum continuous power (\code{maxPowerAero}). Otherwise, it will compute the burst maximum power.

}
\value{
numeric value of mechanical power
}
\references{
Pennycuick, C. J. (2008). \emph{Modelling the flying bird.} Amsterdam, The Netherlands: Elsevier.
}
\author{
Marco Klein Heerenbrink
}
\note{
Available power is determined as a constant for the muscles. In reality the muscle power output depends on strainrate and stress, which in vertebrates are directly linked to wingbeat kinematics and aerodynamic loads.

The model of \cite{Pennycuick 2008} uses an isometric stress of 560 kN/m2. This is much higher than any measured value. A more reasonable yet still very optimistic value would be 400 kn/m2, which is the default value assigned by the \code{\link{Bird}} constructor.
}
\seealso{
\code{\link{Bird}}
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

## for specified maximum continuous power:
power.max.continuous <- 0.8*power.max
power.max.burst <- computeAvailablePower(myBird,power.max.continuous)
print(power.max.burst)
#   [1] 5.466625

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
